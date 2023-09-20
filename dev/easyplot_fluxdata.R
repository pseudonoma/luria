# plot_fluxdata()
# function that takes output(s) of the fluxxer.R script and combines them into a single plot

easyplot_fluxdata <- function(dataPath, projNames, repTag, theme = NULL, export = FALSE){
  # ARGS:
  # dataPath - path to where the source (fluxxer-produced) files are
  # projNames - eg FLUCTEST1; names of projects with the paired pooled/unpooled files
  # repTag - how reps are named. Must have space in the case of eg. "Rep 1", "Rep 2"
  # theme - made by manually_theme() from the aesthetics package; in grorates, part of "overlay"
  # export - whether to export the plot or display it. check aesthete package
  
  # DEBUG:
  # dataPath <- "./data/analyzed"
  # projNames <- c("FLUCTEST1", "FLUCTEST2")
  # repTag <- "Rep "
  # project <- "FLUCTEST1"
  
  for(project in projNames){
    
    # import data
    unpooledData <- read.csv(paste0(dataPath, "/", project, "_unpooled.output.csv"), header = T)
    pooledData <- read.csv(paste0(dataPath, "/", project, "_pooled.output.csv"), header = T)
    
    # make combined data from pooled & unpooled data
    combinedData <- rbind(unpooledData, pooledData)
    combinedData$strain[which(combinedData$strain == "AB3")] <- "Pooled"
    
    # construct factor order for passing to plotter
    idealOrder <- paste0(repTag, 1:50)
    badOrder <- unpooledData$strain
    goodOrder <- c("Pooled", idealOrder[idealOrder %in% badOrder])
    
    # plot and display
    plot <- plot_mutrates(combinedData, levelOrder = goodOrder)
    plot
    
    # optional: overlay
    if(!is.null(theme)){
      plot <- overlay_means_classic(plotObject = plot, theme = theme, yLabel = "Mutation rate")
    }
    
    # optional: export
    if(export){
      plotName <- paste0(project, "_combined")
      auto_export(plot, plotName, dimensions = "postcard")
    }
    
  }
  
  
  return(invisible())
  
}









