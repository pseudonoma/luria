# possible export pathing function for calling within the wrangle_() functions

prep_export <- function(mode = NULL){
  
  # Define top-level path
  if(dir.exists("./output")){
    warning("Default output folder already exists - files may have been overwritten!")
  }
  outputPath <- "./output"
  dir.create(outputPath, showWarnings = FALSE)
  
  # Create the standard folders
  if(mode == "wrangled"){
    wrangledPath <- paste0(outputPath, "/wrangled")
    if(dir.exists(wrangledPath)){
      warning("The specified output folder already exists! Files may have been overwritten.")
    }
    dir.create(wrangledPath, showWarnings = FALSE)
    
  } else if(mode == "analyzed"){
    analyzedPath <- paste0(outputPath, "/analyzed")
    if(dir.exists(analyzedPath)){
      warning("The specified output folder already exists! Files may have been overwritten.")
    }
    dir.create(analyzedPath, showWarnings = FALSE)
    
  } else if(is.null(mode)){
    stop("prep_export mode is NULL! This shouldn't have happened, please report this bug.")
  }

}
