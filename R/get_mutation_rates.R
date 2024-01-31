# get_mutation_rates()
# takes fluxxer.R outputs, produces plots and/or extracts mutation rates

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
    
    # to do: detect repTag automatically
    
    
    # construct factor order for passing to plotter
    idealOrder <- paste0(repTag, 1:50)
    badOrder <- unpooledData$strain
    goodOrder <- c("Pooled", idealOrder[idealOrder %in% badOrder])
    
    # plot and display
    plot <- plot_mutrates(combinedData, levelOrder = goodOrder)
    plot
    
    # get mutation rate
    mutRates[project] <- pooledData$mu
    
    ##### pkg "poirot" functionality - currently disabled #####
    # # poirot: overlay
    # if(!is.null(theme)){
    #   plot <- tryCatch(
    #     overlay_dotplot_borderless(plotObject = plot, theme = theme, yLabel = "Mutation rate"),
    #     error = function(e){
    #       message("Aesthetic overlays are part of package \"poirot\" and not currently available :(")
    #     }
    #   )
    # }
    
    # # poirot: export plot
    # if(export == "plot" | export == "both"){
    #   plotName <- paste0(project, "_combined")
    #   tryCatch(
    #     auto_export(plot, plotName, dimensions = "postcard"),
    #     error = function(e){
    #       message("Plot exporting is part of package \"poirot\" and not currently available :(
    #               Save your plot by using the Export button in RStudio.")
    #     }
    #   )
    # }
    
    ### end pkg "poirot" functions ###
    
    # Handle export
    prep_export(mode = "analyzed")
    exportPath <- "./output/analyzed"
    
    # 1. Plots (actually just poirot "postcard" standard)
    if(export == "plot" | export == "both"){
     
    }
    
  }
  
  # 2. Mutation rates
  if(export == "mutrates" | export == "both"){
    saveRDS(mutRates, paste0(exportPath, "/mutrate.rds"))
    message("Mutation rates have been saved in /analyzed/mutrate.rds")
  }
  
  
  message("\nAll done. Have a fluffy day.")
  
  return(invisible())
  
} # end get_mutation_rates(). #