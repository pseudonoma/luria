# wrangle_intermediates()

# find valid fluxxer-ready inputs and wrangle to standard pool/unpool formats
wrangle_intermediates <- function(dataPath, poolAs = NULL){
  rawPath <- paste0(dataPath, "/raws")
  raws <- dir(rawPath)
  
  # Find intermediates, which must be in CSV format
  csvList <- grep(".csv", raws, value = TRUE)
  if(length(csvList) < 1){
    stop("No valid inputs detected. Make sure intermediate files are in CSV format.")
  }
  
  # Check every CSV for correct col headers
  headers <- c("strain", "plate", "fraction", "CFU")
  for(file in csvList){
    df <- read.csv(paste0(rawPath, "/", file))
    if(!all(headers %in% names(df))){
      stop(paste0(file, " does not have the correct column names."))
    }
  }
  
  # Produce pooled/unpooled formats
  for(file in csvList){
    
    # crudely separate the pooled/unpooled data
    pooledData <- read.csv(paste0(rawPath, "/", file))
    unpooledData <- read.csv(paste0(rawPath, "/", file))
    if(is.null(poolAs)){
      pooledData$strain <- "Combined"
    } else {
      pooledData$strain <- poolAs
    }
    
    # export
    outputPath <- paste0(dataPath, "./wrangled")
    dir.create(outputPath, showWarnings = FALSE)
    fileName <- sub(".csv", "", basename(csvList))
    write.csv(unpooledData, paste0(outputPath, "/", fileName, "_unpooled.csv"), row.names = FALSE)
    write.csv(pooledData, paste0(outputPath, "/", fileName, "_pooled.csv"), row.names = FALSE)
    
  }
  
  message("\nDone. Check /wrangled/ for the wrangled .csv files.\n")
  
}
