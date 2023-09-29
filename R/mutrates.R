# get_mutation_rates()
# takes fluxxer.R outputs, plots, and/or extracts mutation rates

# required from "poirot"
# overlay_means_classic()
# auto_export()

get_mutation_rates <- function(dataPath, projNames, repTag, theme = NULL, export = "both"){
  # ARGS:
  # dataPath - path to where the source (fluxxer-produced) files are
  # projNames - eg FLUCTEST1; names of projects with the paired pooled/unpooled files
  # repTag - how reps are named. Must have space in the case of eg. "Rep 1", "Rep 2"
  # theme - made by manually_theme() from the aesthetics package; in grorates, part of "overlay"
  # export - whether to export the plot or display it. check poirot package
  
  # DEBUG:
  # dataPath <- "./data/analyzed"
  # projNames <- c("FLUCTEST1", "FLUCTEST2")
  # repTag <- "Rep "
  # project <- "FLUCTEST1"
  
  mutRates <- c()
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
    
    # get mutation rate
    mutRates[project] <- pooledData$mu
    
    # optional: overlay
    if(!is.null(theme)){
      plot <- tryCatch(
        overlay_dotplot_borderless(plotObject = plot, theme = theme, yLabel = "Mutation rate"),
        error = function(e){
          message("Aesthetic overlays are part of package \"poirot\" and not currently available :(")
        }
      )
    }
    
    # Handle export (plots)
    if(export == "plot" | export == "both"){
      plotName <- paste0(project, "_combined")
      tryCatch(
        auto_export(plot, plotName, dimensions = "postcard"),
        error = function(e){
          message("Plot exporting is part of package \"poirot\" and not currently available :(")
        }
        
      )
      
    }
    
  }
  
  # Handle export (mutation rates)
  if(export == "mutrates" | export == "both"){
    dir.create("./data")
    saveRDS(mutRates, "./data/mutrate.rds")
    message("Mutation rates have been saved in /data/mutrate.rds")
  }
  
  
  message("\nAll done. Have a fluffy day.")
  
  return(invisible())
  
} # end get_mutation_rates(). #


# core plotting function

plot_mutrates <- function(data, levelOrder){
  # ARGS:
  # data - the data to plot. probably always combined data
  # levelOrder - the levels to refactor by, including if reps are missing
  
  plot <- ggplot(data, aes(x = forcats::fct_relevel(strain, levelOrder), 
                           y = mu)) +
    geom_point(shape = 16, size = 2, position = position_dodge(width = 0.75)) +
    geom_errorbar(aes(ymin=CI.95.lower, ymax=CI.95.higher), width = 0.15, linewidth = 0.4,
                  position = position_dodge(width = 0.75)) +
    scale_y_log10() +
    xlab(NULL) +
    ylab("Mutation rate") +
    annotation_logticks(sides = "l") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  
  
  return(plot)
  
}

