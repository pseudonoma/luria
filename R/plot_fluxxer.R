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

plot_fluxxer <- function(file){
  # ARGS:
  # file - if specified, changes mode to "single". Then it will plot whatever you give it, but
  #        wrangled so the replicate name factoring is sensible.
  
  # ISSUES
  
  ###
  
  # Set mode explicitly once again
  runMode <- ifelse(is.null(file), yes = "standard", no = "single")
  
  # Prep export folder (in std mode it's redundant this time though)
  prep_export(mode = "analyzed")
  outputPath <- "./output/analyzed"
  
  ##### Single-file mode #####
  if(runMode == "single"){
    
    # wrangle & plot
    plotData <- wrangle_plot_data(file = file)
    plot <- plot_mutrates(data = plotData$data, levelOrder = plotData$levels, log = plotData$log)
    
    # export
    export_mut_plot(plot, project, exportPath)
    
  ##### Standard pipeline mode #####  
  } else if(runMode == "standard"){
    
    # Test for standard pipeline dirs
    inputPath <- "./output/wrangled"
    if(!dir.exists(inputPath)){
      stop("Folder /wrangled/ not found. In standard mode, the pipeline must be run in order.")
    }
    
    # grep "project name" file prefixes
    pooledList <- grep("_pooled.output.csv", dir("./output/analyzed"), value = TRUE)
    projectNames <- sub("_pooled.output.csv$", "", pooledList)
    
    # loop and plot over all projects
    for(project in projectList){
      plotData <- wrangle_plot_data(projectName = project)
      plot <- plot_mutrates(data = plotData$data, levelOrder = plotData$levels, log = plotData$log)
      
      # export
      export_mut_plot(plot, project, exportPath)
      
    }
  }
  
  
}
