#' Helper function for converting sheet data.
#'
#' This function is called by [`wrangle_raw_data()`] inside the by-sheet loop. It takes a fluxxer-
#' formatted dataframe and uses the arguments to populate the rest of the rows, before returning
#' the dataframe.
#'
#' @param currentData The current data to be populated and returned, usually `currentFrame`.
#' @param currentCounts *Count* data from the current sheet.
#' @param currentMutants *Selective* data from the current sheet.
#' @param countFraction *fraction* value for the *Count* populations; generated from `countFraction`
#' in the wrapper function.
#' @param countPops The number of *Count* populations used.
#'
#' @return
#' A dataframe of identical dimensions and headers as the input.
#'
#' @keywords internal

populate_rows <- function(currentData, currentCounts, currentMutants, countFraction, countPops){

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

}


#' Helper function for reordering replicate names.
#'
#' This function is called by [`wrangle_plot_data()`]. It assumes that each unpooled replicate is
#' named in the format `<rep><number>`, where `<rep>` can be any string, with or without spaces, as
#' long as it's identical for all replicates. Because it takes the pooled replicate prefix
#' explicitly via `pooledPrefix`, theoretically any number of nonconforming rep names can be used
#' as long as they are all explicitly defined in a vector - although there should really only be
#' one pooled rep.
#'
#' @param data The plotting data.
#' @param pooledPrefix The pooled replicate name.
#'
#' @return
#' A string vector of the replicate names in the correct order, with `pooledPrefix` first.
#'
#' @keywords internal

refactor_reps <- function(data, pooledPrefix = NULL){

  # Extract replicate prefix (as long as the format is <prefix><number>)
  reps <- data$strain[which(data$strain != pooledPrefix)]
  repPrefix <- unique(gsub("[[:digit:]]*$", "", reps))
  if(length(repPrefix) > 1){
    stop("One or more replicates are not named consistently.")
  }

  # Automatically construct correct factor order for passing to plotter
  idealOrder <- paste0(repPrefix, 0:1000)
  badOrder <- reps
  goodOrder <- c(pooledPrefix, idealOrder[idealOrder %in% badOrder])


  return(goodOrder)

}


#' Helper function to check for the annotation_logticks error.
#'
#' This function is called by [`wrangle_plot_data()`], and exists because because
#' `ggplot2::annotation_logticks` shits itself if the upper and lower CI differs by less than one
#' power of 10, but the error can't be easily caught once plotting begins. This function simply
#' checks of the upper and lower CI have a sufficient range, and returns a flag that is ultimately
#' passed to [`plot_mutrates()`] to enable (or disable) logtics.
#'
#' @details
#' If this doesn't work and it still throws an error, it might be because `annotation_logticks()`
#' still doesn't like if max(range) - min(range) >= 1 if they don't actually span two logticks
#' in the final plot. If that's the case, a more conservative range should be used, i.e.
#' floor(max(range)) - ceiling(min(range)), which collapses the difference to zero in this case.
#'
#' @param data The plotting data.
#'
#' @return
#' A logical value indicating if logticks should be used.
#'
#' @keywords internal

test_logticks <- function(data){

  range <- c(data$CI.95.lower, data$CI.95.higher)
  if(log(max(range)) - log(min(range)) >= 1){
    logMode <- TRUE
  } else {
    logMode <- FALSE
  }


  return(logMode)

}


#' Helper function for setting up and checking folder structure and pathing.
#'
#' This function is called by all frontend function that produce file outputs, namely
#' [`wrangle_raw_data()`], [`wrangle_clean_data()`], [`run_fluxxer()`], and [`plot_fluxxer()`].
#' It always attempts to create the top-level output folder `./output/` whenever it's called,
#' and enforces output folder structure by (a) checking if the output subfolder exists and if
#' its contents can be overwritten, and (b) returning the output path for use by the calling
#' function so it only has to be defined once. `overwrite` is determined by the user in the
#' frontend functions; if `FALSE`, files cannot be written to the output folder if it contains
#' valid files.
#'
#' @param mode Defined by the calling function to determine what output folder is appropriate and
#' how to handle it. Takes values `"wrangled"`, `"analyzed"`, `"plots"`, or `"mutrates"`.
#' @param overwrite If `TRUE`, output file(s) will be saved to the appropriate folder whether or not
#' existing file(s) are replaced.
#' Defaults to `FALSE`.
#'
#' @return
#' A string indicating the output folder path.
#'
#' @keywords internal

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
    if(isFALSE(overwrite)){ # if do not overwrite
      if(length(dir(wrangledPath)) > 0){ # and output folder has files
        hasCleanFiles <- any(grepl("_pooled.csv$", dir(wrangledPath)) |
                               grepl("_unpooled.csv$", dir(wrangledPath)))
      } else {
        hasCleanFiles <- FALSE
      }
      if(hasCleanFiles){ # are any of the files pipeline files?
        # if yes, then we do not want to overwrite pipeline files at this time.
        stop("The output folder /wrangled/ has files in it! Delete or move the folder and try again.")
      }
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


#' Helper function saving mutation rate plots.
#'
#' This function is called by [`plot_fluxxer()`]. All it does is save a plot in PNG and PDF
#' format in "postcard" dimensions (6" x 8") to the specified output folder.
#'
#' @param plot The ggplot object to save.
#' @param prefix The name of the file, which should be project name if handled correctly by the
#' calling function.
#' @param outputPath The path of the output folder. Also handled by the calling function.
#'
#' @keywords internal

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
