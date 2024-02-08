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
