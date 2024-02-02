#' Reduced version of auto_intermediates()
#' Runs on a single CSV supplied via <dataFile>, turns it into standard pooled/unpooled format
#'   as if it came from wrangle_fluxdata().

wrangle_clean_data <- function(dataFile, poolAs = NULL, exclude = NULL, 
                               saveAs = NULL, overwrite = FALSE){
  # ARGS:
  # dataFile - Should be a single file? If saveAs is option, then files should be passed indiv'lly
  # poolAs - Name to replace strain name for pooling.
  # exclude - Individual strain names to drop; applied before pooling etc.
  # saveAs - <saveAs>_unpooled.csv
  
  ###
  
  # Check the CSV file has correct headers
  headers <- c("strain", "plate", "fraction", "CFU")
  df <- read.csv(dataFile)
  if(!all(headers %in% names(df))){
    stop("The required columns are missing or incorrectly named. Check your file and try again.")
  }
  
  # Crudely separate pooled/unpooled data, sans excluded strains
  pooledData <- df[!(df$strain %in% exclude), ]
  unpooledData <- df[!(df$strain %in% exclude), ]
  if(is.null(poolAs)){
    pooledData$strain <- "Combined"
  } else {
    pooledData$strain <- poolAs
  }
  
  # Handle export pathing/name overrides
  prep_export(mode = "wrangled", overwrite)
  exportPath <- "./output/wrangled"
  
  # Construct export filename
  if(is.null(saveAs)){
    # extract basename & construct exportName
    baseName <- sub(".csv", "", basename(dataFile))
    exportName <- paste0(exportPath, "/", baseName)
  } else {
    # or construct using <saveAs> value
    exportName <- paste0(exportPath, "/", saveAs)
  }
  
  # Write file and report
  write.csv(unpooledData, paste0(exportName, "_unpooled.csv"), row.names = FALSE)
  write.csv(pooledData, paste0(exportName, "_pooled.csv"), row.names = FALSE)
  
  message("\nDone. Check /wrangled/ for the wrangled .csv files.\n")
  
  
}