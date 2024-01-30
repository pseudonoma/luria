# test function that handles in-place wrangling & refactoring, and passes
# a clean data object for plotting.

# This probably shouldn't have a loop internally, and instead be called in a loop by a wrapper
# that also calls the plotter in a loop.

###

wrangle_plot_data <- function(dataPath, project, repTag){
  # ARGS:
  # data - not sure this is actually used
  # project - each pair of pooled/unpooled data will have a prefix. Best approach is prolly
  #             to grep them somehow, since passing them explicitly is kinda weird
  # repTag - What each replicate is called. This should also prolly be grepped, maybe by looking
  #          at all the reps and looking for whatever is common to all of them?
  
  ###
  
  # Define dataPath (should be implied? unless this can be called outside of pipeline?)
  dataPath <- "./output/analyzed"
  
  # import data
  unpooledData <- read.csv(paste0(dataPath, "/", project, "_unpooled.output.csv"), header = T)
  pooledData <- read.csv(paste0(dataPath, "/", project, "_pooled.output.csv"), header = T)
  
  # make combined data from pooled & unpooled data
  combinedData <- rbind(unpooledData, pooledData)
  combinedData$strain[which(combinedData$strain == "AB3")] <- "Pooled"
  
  # to do: detect repTag automatically
  
  
  # construct factor order for passing to plotter
  idealOrder <- paste0(repTag, 1:50)
  badOrder <- unpooledData$strain
  goodOrder <- c("Pooled", idealOrder[idealOrder %in% badOrder])
  
  # Construct export object
  exportObject <- list(data = combinedData, levels = goodOrder)
  
  
  return(exportObject)
  
}


