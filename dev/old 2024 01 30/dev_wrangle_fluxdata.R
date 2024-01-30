# developmental
# upgrade of wrangle_fluxdata()
# remember to port over the docs.

wrangle_fluxdata <- function(dataFile, countPops, countFract = c(P = 200, C = 200, D = 1E5),
                             poolAs = NULL, exclude = "Summary", save = NULL){
  
  # Prep some variables
  allSheets <- openxlsx::getSheetNames(dataFile)
  sheetNames <- allSheets[!allSheets %in% exclude] # only unexcluded sheet names
  countFraction <- countFract["P"]/(countFract["C"] * countFract["D"])
  if(is.na(countFraction)){
    stop("Couldn't calculate Count plating fraction. Check that countVars is a named vector!")
  }
  
  # Prep the two master dfs
  allUnpooled <- data.frame()
  allPooled <- data.frame()
  
  # Loop over sheets and fill the master dfs
  for(sheet in sheetNames){
    
    # extract the appropriate sheets sheets and slice out the chunks
    currentCounts <- openxlsx::read.xlsx(dataFile, sheet = sheet,
                                         rows = 3:(nrow(openxlsx::read.xlsx(dataFile, sheet = sheet)) + 1),
                                         cols = 2:4) # ignore 10E-6 counts
    currentMutants <- openxlsx::read.xlsx(dataFile, sheet = sheet,
                                          rows = 2:(nrow(openxlsx::read.xlsx(dataFile, sheet = sheet)) + 1),
                                          cols = 7:8)
    currentCounts$mean <- rowMeans(currentCounts) # compute count plate means
    
    # prep the two streams of current dfs
    if(is.null(poolAs)){
      poolStrain <- "Combined"
    } else {
      poolStrain <- poolAs
    }
    unpooledData <- data.frame(strain = rep(sheet, nrow(currentCounts) + (60 - countPops)),
                               plate = NA, fraction = NA, CFU = NA)
    pooledData <- data.frame(strain = rep(poolStrain, nrow(currentCounts) + (60 - countPops)),
                             plate = NA, fraction = NA, CFU = NA)
    
    # run the populating function
    unpooledData <- populate_rows(unpooledData, currentCounts, currentMutants, countFraction, countPops)
    pooledData <- populate_rows(pooledData, currentCounts, currentMutants, countFraction, countPops)
    
    # append both streams of current dfs to their master dfs and report
    allUnpooled <- rbind(allUnpooled, unpooledData)
    allPooled <- rbind(allPooled, pooledData)
    cat(paste0("Sheet ", "\"", sheet, "\" ", "done.\n"))
  
  } # loop exit #
  
  # Handle export pathing
  if(is.null(save)){
    message("\nNo output folder defined, creating default folder /analyzed/")
    if(dir.exists("./analyzed")){
      warning("The specified output folder already exists! Files may have been overwritten.")
    }
    dir.create("./analyzed", showWarnings = FALSE)
    outputPath <- "./analyzed"
  } else {
    if(dir.exists(save)){
      warning("The specified output folder already exists! Files may have been overwritten.")
    }
    dir.create(save, showWarnings = FALSE)
    outputPath <- save
  }
  
  # Export and report
  fileName <- sub(".xlsx", "", basename(dataFile))
  write.csv(allUnpooled, paste0(outputPath, "/", fileName, "_unpooled.csv"), row.names = FALSE)
  write.csv(allPooled, paste0(outputPath, "/", fileName, "_pooled.csv"), row.names = FALSE)
  
  message("Completed. Check the output folder for the wrangled .csv files.\n")
  
  
} # end wrangle_fluxdata().
