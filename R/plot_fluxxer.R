# plotting code, v2, 31/01/24
# supercedes the previous implementation, get_mutation_rates()

# Goals:
# Two-mode operation, like run_fluxxer, with single-file and standard pipeline modes.
# In both modes, will sensibly understand how to refactor replicates
#     Which means being able to parse repTags or such, and refactor by number. Letters not allowed.
# In "single", will simply take the file, (possbily) check for validity, and plot.
# In "standard", will 
#   (a) check for dirs,
#   (b) understand "project" codes (essentially outputPrefixes appended by fluxxer call),
#   (c) pair pooled/unpooled data by these codes,
#   (d) call the wrangler func to produce (i) combined data and (ii) parse rep order intelligently,
#   (e) make one plot per project with combined data and correctly factored reps.

###

plot_fluxxer <- function(file){
  # ARGS:
  # file - if specified, changes mode to "single". Then it will plot whatever you give it, but
  #        wrangled so the replicate name factoring is sensible.
  
  ###
  
  # Set mode explicitly once again
  runMode <- ifelse(is.null(file), yes = "standard", no = "single")
  
  # Test for standard pipeline dirs
  inputPath <- "./output/wrangled"
  if(!dir.exists(inputPath)){
    stop("Folder /wrangled/ not found. In standard mode, the pipeline must be run in order.")
  }
  
  # Prep export folder (mostly for the benefit of "single" mode)
  prep_export(mode = "analyzed")
  outputPath <- "./output/analyzed"
  
  if(runMode == "single"){
    # plot
    plotData <- wrangle_plot_data(projectName = project, mode = "single") # mode = runMode?
    plot <- plot_mutrates(plotData[["data"]], plotData[["levels"]])
    
    # export
    export_mut_plot()
    
  } else if(runMode == "standard"){
    # grep "project names"
    pooledList <- grep("_pooled.output.csv", dir("./output/analyzed"), value = TRUE)
    projectNames <- sub("_pooled.output.csv$", "", pooledList)
    
    # loop and plot over all projects
    for(project in projectList){
      plotData <- wrangle_plot_data(projectName = project, mode = "standard")
      plot <- plot_mutrates(plotData[["data"]], plotData[["levels"]])
      
      # export
      export_mut_plot(plot, project, exportPath)
    }
    
    
  }

  
  
}