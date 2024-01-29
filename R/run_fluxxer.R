# run_fluxxer()
# Wrapper function that neatly calls calculateMutRate() on all files in /wrangled/.
# Also sets up some standard pathing etc. before calling calculateMutRate().

run_fluxxer <- function(dataPath = NULL, poolAs = NULL){
  
  # ARGS:
  # dataPath - core data folder containing /raws/, /wrangled/, etc.
  # poolAs - What to name the pooled strain. Defaults to "Combined".
  
  ###
  
  # Check for previous step in the pipeline
  inputPath <- paste0(dataPath, "/wrangled")
  if(!dir.exists(paste0(dataPath, "/wrangled"))){
    stop("Folder /wrangled/ expected but not found, did you run the pipeline correctly?")
  }
  
  # Create output folder
  prep_export(mode = "analyzed")
  # outputPath <- paste0(dataPath, "/analyzed")
  # if(dir.exists(outputPath)){
  #   warning("The output folder /analyzed/ already exists - files may have been overwritten.")
  # } else {
  #   dir.create(outputPath, showWarnings = FALSE)
  # }
  
  # Define pooled/unpooled filenames
  pooledList <- grep("_pooled", dir(inputPath), value = TRUE)
  unpooledList <- grep("_unpooled", dir(inputPath), value = TRUE)
  
  # Run core fluxxer.R function on pooled, then unpooled, data
  for(file in pooledList){
    baseName <- sub(".csv", "", file)
    calculateMutRate(dataPath,
                     filename = paste0(dataPath, "/wrangled/", file), 
                     output_prefix = baseName, 
                     comparisons = FALSE)
  }
  for(file in unpooledList){
    baseName <- sub(".csv", "", file)
    calculateMutRate(dataPath,
                     filename = paste0(dataPath, "/wrangled/", file), 
                     output_prefix = baseName, 
                     comparisons = TRUE)
  }
  
  message("\nDone. Check /analyzed/ for outputs.\n")
  
}