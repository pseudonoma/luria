# test function that handles in-place wrangling & refactoring, and passes
# a clean data object for plotting.

# This probably shouldn't have a loop internally, and instead be called in a loop by a wrapper
# that also calls the plotter in a loop.

###

wrangle_plot_data <- function(file = NULL, projectName = NULL){
  # ARGS:
  # projectName - each pair of pooled/unpooled data will have a prefix. Best approach is prolly
  #               to grep them somehow, since passing them explicitly is kinda weird.
  # mode - "single" or "standard", to be assigned explicitly by plot_flux(). "Single" will not
  #        have pooled/unpooled.
  
  # DEBUG
  
  ###
  
  # Detect mode
  if(!is.null(file) & is.null(projectName)){
    runMode <- "single"
  } else if(is.null(file) & !is.null(projectName)){
    runMode <- "standard"
  } else {
    stop("That's odd, file XOR project should be NULL. Please report this bug.")
  }
  
  # Begin wrangle
  if(runMode == "single"){
    
    # Load data & refactor reps
    singleData <- read.csv(file, header = T)
    goodOrder <- refactor_reps(singleData)
    
    # Handle ggplot2::annotation_logticks() error condition
    range <- c(singleData$CI.95.lower, singleData$CI.95.higher)
    if(log(max(range)) - log(min(range)) >= 1){
      logMode <- TRUE
    } else {
      logMode <- FALSE
    }
    # Note for later: 
    # This is because annotation_logticks shits itself if max-min is less than one power of 10.
    # If this doesn't work and it still throws an error, it might be because annotation_logticks()
    # still doesn't like if max(range) - min(range) >= 1 if they don't actually span two logticks
    # in the final plot. If that's the case, a more conservative range should be used, i.e.
    # floor(max(range)) - ceiling(min(range)), which collapses the difference to zero in this case.
    
    # Construct export object
    exportObject <- list("data" = singleData, "levels" = goodOrder, "log" = logMode)
   
  } else if(runMode == "standard"){
    
    # Load data & refactor reps
    unpooledData <- read.csv(paste0(dataPath, "/", projectName, suffix), header = T)
    goodOrder <- refactor_reps(unpooledData)
    
    # Import the pooled data and combine with unpooled
    pooledData <- read.csv(paste0(dataPath, "/", projectName, "_pooled.output.csv"), header = T)
    combinedData <- rbind(unpooledData, pooledData)
    combinedData$strain[which(combinedData$strain == "AB3")] <- "Pooled"
    
    # Construct export object
    exportObject <- list("data" = combinedData, "levels" = goodOrder, "log" = logMode)
    
  }
  
  
  return(exportObject)
  
}
