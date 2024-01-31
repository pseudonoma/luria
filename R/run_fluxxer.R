# run_fluxxer()
# Wrapper function that neatly calls calculateMutRate().
# Also sets up some standard pathing etc. before calling calculateMutRate().

run_fluxxer <- function(file = NULL, comparisons = TRUE){
  
  # ARGS:
  # filePath - the individual file to run on; will trigger "single" mode if anything supplied.
  # comparisons - logical, defaults to TRUE for use in "single" mode cmr() call. In "standard"
  #               mode it does nothing. Not sure if this is actually useful but whatever man.
  
  ###
  
  # Set mode explicitly for my own sanity
  runMode <- ifelse(is.null(file), yes = "standard", no = "single")
  
  # Create standard output folders regardless of mode
  prep_export(mode = "analyzed")
  outputPath <- "./output/analyzed"
  
  ##### Single-file mode #####
  if(runMode == "single"){
    
    # test for file validity
    if(file_test("-d", file)){
      stop("Supplied file appears to be a folder. If running the standard pipeline, do not supply a filename.")
    }
    
    # run core fluxxer function
    calculateMutRate(filename = file,
                     outputPath, 
                     output_prefix = baseName, 
                     comparisons)
  } # end single mode
  
  ##### Standard pipeline mode #####
  if(runMode == "standard"){
    
    # Test for standard pipeline dirs
    inputPath <- "./output/wrangled"
    if(!dir.exists(inputPath)){
      stop("Folder /wrangled/ not found. In standard mode, the pipeline must be run in order.")
    }
    
    # Define pooled/unpooled filenames
    pooledList <- grep("_pooled", dir(inputPath), value = TRUE)
    unpooledList <- grep("_unpooled", dir(inputPath), value = TRUE)
    
    # Run core fluxxer function on pooled, then unpooled, data
    for(file in pooledList){
      baseName <- sub(".csv", "", file)
      calculate_mut_rate(filename = paste0(inputPath, "/", file),
                         outputPath,
                         outputPrefix = baseName, 
                         comparisons = FALSE)
    }
    for(file in unpooledList){
      baseName <- sub(".csv", "", file)
      calculate_mut_rate(filename = paste0(inputPath, "/", file),
                         outputPath,
                         outputPrefix = baseName, 
                         comparisons = TRUE)
    }
  } # end standard mode
 
  
  message("\nDone. Check /analyzed/ for outputs.\n")
  
}