# test function that handles in-place wrangling & refactoring, and passes
# a clean data object for plotting.

# This probably shouldn't have a loop internally, and instead be called in a loop by a wrapper
# that also calls the plotter in a loop.

###

wrangle_plot_data <- function(projectName, mode = NULL){
  # ARGS:
  # projectName - each pair of pooled/unpooled data will have a prefix. Best approach is prolly
  #               to grep them somehow, since passing them explicitly is kinda weird.
  # mode - "single" or "standard", to be assigned explicitly by plot_flux(). "Single" will not
  #        have pooled/unpooled.
  
  ###
  
  # Define dataPath (should be implied? unless this can be called outside of pipeline?)
  dataPath <- "./output/analyzed"
  
  # Import unpooled data first
  unpooledData <- read.csv(paste0(dataPath, "/", projectName, "_unpooled.output.csv"), header = T)
  
  # Extract replicate prefix (as long as the format is <prefix><number>)
  reps <- unpooledData[, "strain"]
  repPrefix <- unique(gsub("[[:digit:]]*$", "", reps))
  if(length(repPrefix) > 1){
    stop("One or more replicates are not named consistently.")
  }
  
  # Automatically construct correct factor order for passing to plotter
  idealOrder <- paste0(repPrefix, 1:1000)
  badOrder <- unpooledData$strain
  goodOrder <- c("Pooled", idealOrder[idealOrder %in% badOrder])
  
  if(mode == "single"){
    # Construct export object
    exportObject <- list("data" = unpooledData, "levels" = goodOrder)
  } else if(mode == "standard"){
    # Import the pooled data and combine with unpooled
    pooledData <- read.csv(paste0(dataPath, "/", projectName, "_pooled.output.csv"), header = T)
    combinedData <- rbind(unpooledData, pooledData)
    combinedData$strain[which(combinedData$strain == "AB3")] <- "Pooled"
    
    # Construct export object
    exportObject <- list("data" = combinedData, "levels" = goodOrder)
  } else if(is.null(mode)){
    stop("mode is NULL! This should not have happened, please report this bug.")
  }
  
  
  return(exportObject)
  
}
