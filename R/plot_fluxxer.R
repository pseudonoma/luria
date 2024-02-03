# plotting code, v2, 31/01/24
# supercedes the previous implementation, get_mutation_rates()

# Goals:
# Two-mode operation, like run_fluxxer, with single-file and standard pipeline modes.
# In both modes, will sensibly understand how to refactor replicates
#     Which means being able to parse repTags or such, and refactor by number. Letters not allowed.
# In "single", will simply take the file, (possbily) check for validity, and plot.
# In "standard", will 
# X  (a) check for dirs
# X  (b) understand "project" codes (essentially outputPrefixes appended by fluxxer call),
# X  (c) pair pooled/unpooled data by these codes,
# X  (d) call the wrangler func to produce (i) combined data and (ii) parse rep order intelligently,
# X  (e) make one plot per project with combined data and correctly factored reps.

###

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
