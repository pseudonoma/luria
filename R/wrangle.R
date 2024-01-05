#' Wrangle and convert fluctuation test data
#'
#' This function wrangles raw fluctuation test data, converting it into a format ready for use
#' with the Barrick Lab's [`fluxxer.R`](https://github.com/barricklab/barricklab/blob/master/fluxxer.R)
#' script. `fluxxer.R` in turn calls functions from the rSalvador package.
#'
#' @import openxlsx
#'
#' @param dataFile The filename of the Excel workbook.
#' @param countPops An integer value indicating how many Count populations were used in each test.
#' This should be one value for all tests, and should not account for failed Count plates.
#' @param countFract A named vector for calculating the fraction plated, F, of the Count populations.
#' Fraction is given in the `fluxxer.R` script as \eqn{F = P\div(CD)}, where P = volume plated,
#' in uL; C = volume of Count culture used for dilution, in uL; and D = dilution factor, expressed
#' as an integer (eg. \eqn{10^{3}}) and **not** a ratio (eg. \eqn{10^{-3}}). The same vector is
#' applied to all tests.
#' The default values are based on the original *A. baylyi* protocol.
#' @param poolAs A string for filling in the Strain column in the pooled data. 
#' Defaults to NULL, in which case the strain is just named "Combined".
#' @param exclude A vector of sheet names to skip. By default the sheet "Summary" is skipped, but
#' more can be added.
#' @param save A pathname for exporting the final CSVs to. Defaults to NULL, in which case the CSVs
#' are exported to a folder called `wrangled`
#'                 
#' @examples
#' wrangle_fluxdata(dataFile = "./data/raws/FLUCTEST 1 2020 09 24.xlsx",
#'                  countPops = 4,
#'                  poolAs = "AB3", 
#'                  exclude = c("Summary", "Rep 0", "Rep 13"), 
#'                  save = "./data/wrangled")
#' @return
#' A tidy CSV with columns `strain`, `plate`, `fraction`, and `CFU`.
#'
#' @export

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
    message("\nNo output folder defined, creating default folder /wrangled/")
    if(dir.exists("./wrangled")){
      warning("The specified output folder already exists! Files may have been overwritten.")
    }
    dir.create("./wrangled", showWarnings = FALSE)
    outputPath <- "./wrangled"
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
  
  message("\nDone. Check /wrangled/ for the wrangled .csv files.\n")
  
  
} # end wrangle_fluxdata().


#' Helper function for converting sheet data
#' 
#' This function is internally called by wrangle_fluxdata.

populate_rows <- function(currentData, currentCounts, currentMutants, countFraction, countPops){
  # ARGS:
  # currentData - usually currentFrame; the current df to be populated and returned
  # currentCounts - Count data from the current sheet
  # currentMutants - Selective data from the current sheet
  # countFraction - fraction value for Count pops; generated from countVars in the wrapper
  
  # 1. Populate Count rows (this fraction defined by user)
  countLength <- nrow(currentCounts)
  currentData$plate[1:countLength] <- "Count"
  currentData$fraction[1:countLength] <- countFraction
  currentData$CFU[1:countLength] <- currentCounts$mean
  
  # 2. Populate Selective rows (this fraction isn't optional; by the protocol it's 1)
  mutantLength <- nrow(currentMutants)
  if(mutantLength == 0){ # even if there's no mutants, you need one more row to fill 0 into
    mutantLength <- 1
  }
  currentData$plate[(countLength+1):(countLength+mutantLength)] <- "Selective"
  currentData$fraction[(countLength+1):(countLength+mutantLength)] <- 200/(200*1) # fract = 1
  if(nrow(currentMutants) == 0){
    currentData$CFU[countLength+1] <- 0 #
  } else{
    currentData$CFU[(countLength+1):(countLength+mutantLength)] <- currentMutants$Count
  }
  
  # 3. Fill the rest of the selective rows with 0 CFUs
  currentData$plate[(countLength + mutantLength + 1):(countLength + (60 - countPops))] <- "Selective"
  currentData$fraction[(countLength + mutantLength + 1):(countLength + (60 - countPops))] <- (200*1)/200
  currentData$CFU[(countLength + mutantLength + 1):(countLength + (60 - countPops))] <- 0
  
  
  return(currentData)
  
} # end populate_rows(). #
