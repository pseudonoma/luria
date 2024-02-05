# Contains:
# plot_fluxxer [no doc]
# plot_mutrates [no doc, DNE]
# extract_mutrates [no doc]

###

# plotting code, v2, 31/01/24
# supercedes the previous implementation, get_mutation_rates()

plot_fluxxer <- function(file = NULL, return.plots = FALSE, overwrite = FALSE){
  # ARGS:
  # file - if specified, changes mode to "single". Then it will plot whatever you give it, but
  #        wrangled so the replicate name factoring is sensible.
  # file should probably be filePath...
  # return.plots - if TRUE, will return the plot object, or a list of plot objects named by project.
  
  # ISSUES
  
  ###
  
  # Set mode explicitly once again
  runMode <- ifelse(is.null(file), yes = "standard", no = "single")
  
  # Prep export folder (in std mode it's redundant this time though)
  outputPath <- prep_export(mode = "plots", overwrite)
  
  ##### Single-file mode #####
  if(runMode == "single"){
    
    # wrangle & plot
    plotData <- wrangle_plot_data(file = file, projectName = NULL)
    plot <- plot_mutrates(data = plotData$data, levelOrder = plotData$levels, log = plotData$log)
    
    # grep "project name" file prefixes
    prefix <- sub(".output.csv$", "", basename(file))
    
    # export
    export_mut_plot(plot, prefix, outputPath)
    message("\nPlot saved to /analyzed/.")
    
    # construct return object (unnecessary, but matches pipeline mode)
    plots <- plot
    
    ##### Standard pipeline mode #####  
  } else if(runMode == "standard"){
    
    # Test for standard pipeline dirs
    inputPath <- "./output/analyzed"
    if(!dir.exists(inputPath)){
      stop("Folder /wrangled/ not found. In standard mode, the pipeline must be run in order.")
    }
    
    # grep "project name" file prefixes
    pooledList <- grep("_pooled.output.csv", dir(inputPath), value = TRUE)
    projectList <- sub("_pooled.output.csv$", "", pooledList)
    
    # loop and plot over all projects
    plots <- list()
    for(projectName in projectList){
      plotData <- wrangle_plot_data(file = NULL, projectName, inputPath)
      plot <- plot_mutrates(data = plotData$data, levelOrder = plotData$levels, log = plotData$log)
      
      # export
      export_mut_plot(plot, prefix = projectName, outputPath)
      
      # construct return object
      plots[[projectName]] <- plot
      
    }
    
    message("\nPlots saved to /analyzed/.")
    
  }
  
  
  if(return.plots){
    return(plots)
  } else {
    return(invisible())
  }
  
}


###

# core plotting function

plot_mutrates <- function(data, levelOrder, log = NULL){
  # ARGS:
  # data - the data to plot. probably always combined data
  # levelOrder - the levels to refactor by, including if reps are missing
  
  # DEBUG
  # data <- plotData$data
  # levelOrder <- plotData$levels
  
  # make most of the plot
  plot <- ggplot(data, aes(x = forcats::fct_relevel(strain, levelOrder), 
                           y = mu)) +
    geom_point(shape = 16, size = 2, position = position_dodge(width = 0.75)) +
    geom_errorbar(aes(ymin=CI.95.lower, ymax=CI.95.higher), width = 0.10, linewidth = 0.4,
                  position = position_dodge(width = 0.75)) +
    scale_y_log10() +
    xlab(NULL) +
    ylab("Mutation rate") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  
  # if this flag is true, logticks can be added and won't cause an error
  if(isTRUE(log)){
    plot <- plot + annotation_logticks(sides = "l")
  }
  
  
  return(plot)
  
}


###

# test function to extract and export mutrates as an RData object
# this was initially used for pkg tapedeck, but is supposed to make mutrate extraction
# more consistent instead of leaving it to copy-pasting.

extract_mutrates <- function(method = "save", overwrite = FALSE){
  
  # Prep output
  outputPath <- prep_export(mode = "mutrates", overwrite)
  
  # Extract
  mutRates <- c()
  for(projectName in projectList){
    pooledData <- read.csv(paste0(inputPath, "/", projectName, "_pooled.output.csv"), header = T)
    mutRates[[projectName]] <- pooledData$mu
  }
  
  # Export
  if(export == "save" | export == "both"){
    timestamp <- format(Sys.Date(), "%Y-%m-%d")
    saveRDS(mutRates, file = paste0(outputPath, "/", "mutrate_", timestamp))
  }
  if(export == "return" | export == "both"){
    
    
    return(mutRates)
    
  }
}
