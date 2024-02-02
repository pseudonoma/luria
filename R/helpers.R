
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


#' prep_export()
#' 
#' Helper function that handles folder structure and pathing checks. Exporting is still handled by
#' the calling function, this one just sets up the folders.

prep_export <- function(mode = NULL, overwrite = FALSE){
  
  # Define top-level path
  outputParent <- "./output"
  if(!dir.exists(outputParent)){
    dir.create(outputParent, showWarnings = FALSE)
    warning("Created ./output/ folder for pipeline results.", call. = FALSE)
  }
  
  # Report overwrite state
  if(overwrite){
    warning("\'overwrite\' is TRUE - if output folders had contents, they may have been replaced.",
            call. = FALSE)
  }
  
  # Create the standard folders
  if(mode == "wrangled"){
    wrangledPath <- paste0(outputParent, "/wrangled")
    if((length(dir(wrangledPath)) > 0) & isFALSE(overwrite)){ # if dir is empty it'll just ignore that it exists
      stop("The output folder /wrangled/ has files in it! Delete or move the folder and try again.")
    }
    dir.create(wrangledPath, showWarnings = FALSE)
    
  } else if(mode == "analyzed"){
    analyzedPath <- paste0(outputParent, "/analyzed")
    if((length(dir(analyzedPath)) > 0) & isFALSE(overwrite)){
      stop("The output folder /analyzed/ has files in it! Delete or move the folder and try again.")
    }
    dir.create(analyzedPath, showWarnings = FALSE)
    
  } else if(is.null(mode)){
    stop("prep_export mode is NULL! This shouldn't have happened, please report this bug.")
  }
  
}