# Contains:
# wrangle_raw_data [fix doc]
# wrangle_clean_data [no doc]
# wrangle_plot_data [no doc, DNE]

###

#' Wrangle and convert fluctuation test data
#'
#' [***REWRITE THIS***]
#' This function wrangles raw fluctuation test data, converting it into a format ready for use
#' with the Barrick Lab's [`fluxxer.R`](https://github.com/barricklab/barricklab/blob/master/fluxxer.R)
#' script. `fluxxer.R` in turn calls functions from the rSalvador package.
#'
#' @import openxlsx
#'
#' @param dataFile The filename of the Excel workbook template.
#' @param countPops An integer value indicating how many Count populations were used in each test.
#' This should be one value for all tests, and should not account for failed Count plates.
#' @param countFract A named vector for calculating the fraction plated, F, of the Count populations.
#' Fraction is given in the `fluxxer.R` script as \eqn{F = P\div(CD)}, where P = volume plated,
#' in uL; C = volume of Count culture used for dilution, in uL; and D = dilution factor, expressed
#' as an integer (eg. \eqn{10^{3}}) and **not** a ratio (eg. \eqn{10^{-3}}). The same vector is
#' applied to all tests.
#' The default values are based on the original *A. baylyi* protocol.
#' @param poolAs What the pooled strain should be named. 
#' Defaults to NULL, in which case the strain is just named "Combined".
#' @param exclude A vector of sheet names to skip. By default the sheet "Summary" is skipped, but
#' more can be added.
#' @param saveAs A filename to save the wrangled data with. 
#' Defaults to NULL, in which case the name of the original raw file will be used.
#'                 
#' @examples
#' wrangle_fluxdata(dataFile = "./data/raws/FLUCTEST 1 2020 09 24.xlsx",
#'                  countPops = 4,
#'                  poolAs = "AB3", 
#'                  exclude = c("Summary", "Rep 0", "Rep 13"), 
#'                  saveAs = "RIF_Aug2023")
#' @return
#' A tidy CSV with columns `strain`, `plate`, `fraction`, and `CFU`.
#'
#' @export

wrangle_raw_data <- function(dataFile, countPops, countFract = c(P = 200, C = 200, D = 1E5),
                             poolAs = NULL, exclude = "Summary", 
                             saveAs = NULL, overwrite = FALSE){
  
  # Prep some variables
  allSheets <- openxlsx::getSheetNames(dataFile)
  sheetNames <- allSheets[!allSheets %in% exclude] # only unexcluded sheet names
  countFraction <- countFract["P"]/(countFract["C"] * countFract["D"])
  if(is.na(countFraction)){
    stop("Couldn't calculate Count plating fraction; check that countFract is a named vector.")
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
  
  # Handle exporting using export helper function
  exportPath <- prep_export(mode = "wrangled", overwrite)
  
  # Handle export filename
  if(is.null(saveAs)){
    # extract default basename & construct exportName
    baseName <- sub(".xlsx$", "", basename(dataFile))
    exportName <- paste0(exportPath, "/", baseName)
  } else {
    # or construct using <saveAs> value
    exportName <- paste0(exportPath, "/", saveAs)
  }
  
  # Write file and report
  write.csv(allUnpooled, paste0(exportName, "_unpooled.csv"), row.names = FALSE)
  write.csv(allPooled, paste0(exportName, "_pooled.csv"), row.names = FALSE)
  
  message("\nDone. Check /wrangled/ for the wrangled .csv files.\n")
  
  
} # end wrangle_fluxdata().



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
  exportPath <- prep_export(mode = "wrangled", overwrite)
  
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




# test function that handles in-place wrangling & refactoring, and passes
# a clean data object for plotting.

# This probably shouldn't have a loop internally, and instead be called in a loop by a wrapper
# that also calls the plotter in a loop.

###

wrangle_plot_data <- function(file = NULL, projectName = NULL, inputPath = NULL){
  # ARGS:
  # projectName - each pair of pooled/unpooled data will have a prefix. Best approach is prolly
  #               to grep them somehow, since passing them explicitly is kinda weird.
  # mode - "single" or "standard", to be assigned explicitly by plot_flux(). "Single" will not
  #        have pooled/unpooled.
  
  # DEBUG
  
  ###
  
  # Detect mode
  if(!is.null(file) & is.null(projectName)){
    runMode <- "single"
  } else if(is.null(file) & !is.null(projectName)){
    runMode <- "standard"
    if(is.null(inputPath)){
      stop(paste0("projectName supplied without inputPath. Please report this bug."))
    }
  } else {
    stop("That's odd, file XOR project should be NULL. Please report this bug.")
  }
  
  # Begin wrangle
  if(runMode == "single"){
    
    # Load data & refactor reps
    singleData <- read.csv(file, header = T)
    goodOrder <- refactor_reps(singleData)
    
    # Handle ggplot2::annotation_logticks() error condition
    logMode <- test_logticks(singleData)
    
    # Construct export object
    exportObject <- list("data" = singleData, "levels" = goodOrder, "log" = logMode)
    
  } else if(runMode == "standard"){
    
    # Import data and combine
    unpooledData <- read.csv(paste0(inputPath, "/", projectName, "_unpooled.output.csv"), header = T)
    pooledData <- read.csv(paste0(inputPath, "/", projectName, "_pooled.output.csv"), header = T)
    combinedData <- rbind(pooledData, unpooledData)
    
    # Extract pooled data "rep" and refactor
    pooledRep <- pooledData$strain
    goodOrder <- refactor_reps(unpooledData, pooledPrefix = pooledRep)
    
    # Handle ggplot2::annotation_logticks() error condition
    logMode <- test_logticks(combinedData)
    
    # Construct export object
    exportObject <- list("data" = combinedData, "levels" = goodOrder, "log" = logMode)
    
  }
  
  
  return(exportObject)
  
}
