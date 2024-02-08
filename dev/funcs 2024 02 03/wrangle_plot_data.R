# test function that handles in-place wrangling & refactoring, and passes
# a clean data object for plotting.

# This probably shouldn't have a loop internally, and instead be called in a loop by a wrapper
# that also calls the plotter in a loop.

###

wrangle_plot_data <- function(file = NULL, projectName = NULL, inputPath = NULL){
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
    if(is.null(inputPath)){
      stop(paste0("projectName supplied without inputPath. Please report this bug."))
    }
  } else {
    stop("That's odd, file XOR project should be NULL. Please report this bug.")
  }
  
  # Begin wrangle
  if(runMode == "single"){
    
    # Load data & refactor reps
    singleData <- read.csv(file, header = T)
    goodOrder <- refactor_reps(singleData)
    
    # Handle ggplot2::annotation_logticks() error condition
    logMode <- test_logticks(singleData)
    
    # Construct export object
    exportObject <- list("data" = singleData, "levels" = goodOrder, "log" = logMode)
   
  } else if(runMode == "standard"){
    
    # Import data and combine
    unpooledData <- read.csv(paste0(inputPath, "/", projectName, "_unpooled.output.csv"), header = T)
    pooledData <- read.csv(paste0(inputPath, "/", projectName, "_pooled.output.csv"), header = T)
    combinedData <- rbind(pooledData, unpooledData)
    
    # Extract pooled data "rep" and refactor
    pooledRep <- pooledData$strain
    goodOrder <- refactor_reps(unpooledData, pooledPrefix = pooledRep)
    
    # Handle ggplot2::annotation_logticks() error condition
    logMode <- test_logticks(combinedData)
    
    # Construct export object
    exportObject <- list("data" = combinedData, "levels" = goodOrder, "log" = logMode)
    
  }
  
  
  return(exportObject)
  
}
