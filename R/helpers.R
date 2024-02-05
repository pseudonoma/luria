# Contains:
# populate_rows [no doc, DNE]
# refactor_reps [no doc, DNE]
# test_logticks [no doc, DNE]
# prep_export [no doc, DNE]
# export_mut_plot [no doc, DNE]



#' Helper function for converting sheet data
#' 
#' This function is internally called by wrangle_fluxdata.

populate_rows <- function(currentData, currentCounts, currentMutants, countFraction, countPops){
  # ARGS:
  # currentData - usually currentFrame; the current df to be populated and returned
  # currentCounts - Count data from the current sheet
  # currentMutants - Selective data from the current sheet
  # countFraction - fraction value for Count pops; generated from countVars in the wrapper
  
  # 1. Populate Count rows (this fraction defined by user)
  countLength <- nrow(currentCounts)
  currentData$plate[1:countLength] <- "Count"
  currentData$fraction[1:countLength] <- countFraction
  currentData$CFU[1:countLength] <- currentCounts$mean
  
  # 2. Populate Selective rows (this fraction isn't optional; by the protocol it's 1)
  mutantLength <- nrow(currentMutants)
  if(mutantLength == 0){ # even if there's no mutants, you need one more row to fill 0 into
    mutantLength <- 1
  }
  currentData$plate[(countLength+1):(countLength+mutantLength)] <- "Selective"
  currentData$fraction[(countLength+1):(countLength+mutantLength)] <- 200/(200*1) # fract = 1
  if(nrow(currentMutants) == 0){
    currentData$CFU[countLength+1] <- 0 #
  } else{
    currentData$CFU[(countLength+1):(countLength+mutantLength)] <- currentMutants$Count
  }
  
  # 3. Fill the rest of the selective rows with 0 CFUs
  currentData$plate[(countLength + mutantLength + 1):(countLength + (60 - countPops))] <- "Selective"
  currentData$fraction[(countLength + mutantLength + 1):(countLength + (60 - countPops))] <- (200*1)/200
  currentData$CFU[(countLength + mutantLength + 1):(countLength + (60 - countPops))] <- 0
  
  
  return(currentData)
  
} # end populate_rows(). #


# helper function that refactors reps. Called by wrangle_plot_data

refactor_reps <- function(data, pooledPrefix = NULL){
  
  # Extract replicate prefix (as long as the format is <prefix><number>)
  reps <- data$strain[which(data$strain != pooledPrefix)]
  repPrefix <- unique(gsub("[[:digit:]]*$", "", reps))
  if(length(repPrefix) > 1){
    stop("One or more replicates are not named consistently.")
  }
  
  # Automatically construct correct factor order for passing to plotter
  idealOrder <- paste0(repPrefix, 1:1000)
  badOrder <- reps
  goodOrder <- c(pooledPrefix, idealOrder[idealOrder %in% badOrder])
  
  
  return(goodOrder)
  
}


# helper function for testing annotation_logticks error condition

test_logticks <- function(data){
  
  # Note for later: 
  # This is because annotation_logticks shits itself if max-min is less than one power of 10.
  # If this doesn't work and it still throws an error, it might be because annotation_logticks()
  # still doesn't like if max(range) - min(range) >= 1 if they don't actually span two logticks
  # in the final plot. If that's the case, a more conservative range should be used, i.e.
  # floor(max(range)) - ceiling(min(range)), which collapses the difference to zero in this case.
  
  range <- c(data$CI.95.lower, data$CI.95.higher)
  if(log(max(range)) - log(min(range)) >= 1){
    logMode <- TRUE
  } else {
    logMode <- FALSE
  }
  
  
  return(logMode)
  
}


#' prep_export()
#' 
#' Helper function that handles folder structure and pathing checks. Exporting is still handled by
#' the calling function, this one just sets up the folders.

prep_export <- function(mode = NULL, overwrite = FALSE){
  
  # Define top-level path
  outputParent <- "./output"
  if(!dir.exists(outputParent)){
    dir.create(outputParent, showWarnings = FALSE)
    message("Creating folder ./output/ for results.")
  }
  
  # Report overwrite state
  if(overwrite){
    warning("Overwriting was active - output folder contents may have been replaced.",
            call. = FALSE)
  }
  
  # Create the standard folders
  if(mode == "wrangled"){
    wrangledPath <- paste0(outputParent, "/wrangled")
    if((length(dir(wrangledPath)) > 0) & isFALSE(overwrite)){ # if dir is empty it'll just ignore that it exists
      stop("The output folder /wrangled/ has files in it! Delete or move the folder and try again.")
    }
    dir.create(wrangledPath, showWarnings = FALSE)
    outputPath <- wrangledPath
    
  } else if(mode == "analyzed"){
    analyzedPath <- paste0(outputParent, "/analyzed")
    if((length(dir(analyzedPath)) > 0) & isFALSE(overwrite)){
      stop("The output folder /analyzed/ has files in it! Delete or move the folder and try again.")
    }
    dir.create(analyzedPath, showWarnings = FALSE)
    outputPath <- analyzedPath
    
  } else if(mode == "plots"){
    plotsPath <- paste0(outputParent, "/analyzed") # pops it back into the same folder
    hasPlots <- grepl("^plot_", dir(plotsPath))
    if(any(hasPlots) & isFALSE(overwrite)){
      stop("The output folder /analyzed/ has plots in it! Delete or move the folder and try again.")
    }
    outputPath <- plotsPath
    
  } else if(mode == "mutrates"){
    mutsPath <- paste0(outputParent, "/analyzed")
    hasMuts <- grepl("^mutrate_", dir(mutsPath))
    if(any(hasMuts) & isFALSE(overwrite)){
      stop("The output folder /analyzed/ has a mutation rate file in it! Delete or move the folder and try again.")
    }
    
  } else if(is.null(mode)){
    stop("prep_export mode is NULL! This shouldn't have happened, please report this bug.")
  }
  
  
  return(outputPath)
  
}


# helper function for exporting mutation rate plots

export_mut_plot <- function(plot, prefix, outputPath){
  
  plotName <- paste0("plot_", prefix)
  exportPath <- outputPath # just for consistency
  ggsave(paste0(plotName, ".pdf"), plot = plot,
         height = 6, width = 8, units = "in", dpi = 300,
         limitsize = TRUE, path = exportPath)
  ggsave(paste0(plotName, ".png"), plot = plot,
         height = 6, width = 8, units = "in", dpi = 600,
         limitsize = FALSE, path = exportPath)
  
  
}