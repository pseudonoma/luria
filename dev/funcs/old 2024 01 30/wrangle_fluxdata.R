#' Wrangle and convert fluctuation test data
#'
#' This function wrangles raw fluctuation test data, converting it into a format ready for use
#' with the Barrick Lab's [`fluxxer.R`](https://github.com/barricklab/barricklab/blob/master/fluxxer.R)
#' script. `fluxxer.R` in turn calls functions from the rSalvador package.
#'
#' @import openxlsx
#'
#' @param testName The filename of the Excel workbook, without the file extension.
#' Place this in a folder named "Raw Data".
#' @param sheetOffset A numeric value denoting the number of sheets at the start of the workbook
#' to skip. Use this eg. if the first sheet is an summary.
#' @param countPops An integer value indicating how many Count populations were used in each test.
#' This should be one value for all tests, and should not account for failed Count plates.
#' @param countVars A named vector for calculating the fraction plated, F, of the Count populations.
#' Fraction is given in the `fluxxer.R` script as \eqn{F = P\div(CD)}, where P = volume plated,
#' in uL; C = volume of Count culture used for dilution, in uL; and D = dilution factor, expressed
#' as an integer (eg. \eqn{10^{3}}) and **not** a ratio (eg. \eqn{10^{-3}}). The same vector is
#' applied to all tests.
#' The default values are based on the original *A. baylyi* protocol.
#' @param dataPooling A logical value indicating if replicates should be pooled together to
#' calculate one mutation rate, or if each replicate constitutes its own fluctuation analysis.
#' TRUE if all replicates should be pooled, FALSE if replicates should be separately calculated.
#' Defaults to NULL.
#' @param excludeList A vector of sheet names, eg. "Rep 1" or "Summary", that should be skipped by
#' the function.
#' Defaults to NULL.
#'
#' @examples
#' wrangleFluxData(testName = "FLUCTEST1 2020 01 01",
#'                 sheetOffset = 1,
#'                 dataPooling = TRUE)
#' @return
#' A tidy CSV with columns `strain`, `plate`, `fraction`, and `CFU`.
#'
#' @export

wrangle_fluxdata <- function(testName,
                             sheetOffset,
                             countPops,
                             countVars = c(P = 200, C = 200, D = 1E5),
                             dataPooling = NULL,
                             excludeList = NULL){
  # assign variables
  fileName <- paste0("./Raw Data/", testName, ".xlsx")
  experimentCount <- length(getSheetNames(fileName)) - sheetOffset

  # create master dataframe for exporting later
  allData <- data.frame()

  # loop over every replicate sheet (hopefully)
  for(sheet in 1:experimentCount){

    # specify current variables
    currentSheet <- sheet + sheetOffset
    currentSheetName <- getSheetNames(fileName)[currentSheet]

    if(!(currentSheetName %in% excludeList)){
      # slice out specific chunks from current sheet
      currentCounts <- read.xlsx(fileName, sheet = currentSheet,
                                 rows = 3:(nrow(read.xlsx(fileName, sheet = currentSheet))+1),
                                 cols = 2:4) # ignore 10E-6 counts
      currentMutants <- read.xlsx(fileName, sheet = currentSheet,
                                  rows = 2:(nrow(read.xlsx(fileName, sheet = currentSheet))+1),
                                  cols = 7:8)
      currentCounts$mean <- rowMeans(currentCounts) # compute count plate means

      # create current working dataframe
      if(is.null(dataPooling)){
        warning("var. <dataPooling> was unspecified, so assumed data is not pooled.")
        dataPooling <- FALSE
      }
      if(isTRUE(dataPooling)){
        currentFrame <- data.frame(strain = rep("AB3", nrow(currentCounts) + (60 - countPops)),
                                   plate = NA, fraction = NA, CFU = NA)
      } else if(isFALSE(dataPooling)){
        currentFrame <- data.frame(strain = rep(currentSheetName, nrow(currentCounts) + (60 - countPops)),
                                   plate = NA, fraction = NA, CFU = NA)
      }

      # specify variables
      countLength <- nrow(currentCounts)
      mutantLength <- nrow(currentMutants)
      if(mutantLength == 0){
        mutantLength <- 1
      }
      # populate count rows
      countFraction <- countVars["P"]/(countVars["C"] * countVars["D"])
      if(is.na(countFraction)){
        stop("Count plating fraction calculation failed. Check that countVars is a named vector!")
      }
      currentFrame$plate[1:countLength] <- "Count"
      currentFrame$fraction[1:countLength] <- countFraction
      currentFrame$CFU[1:countLength] <- currentCounts$mean
      # populate selective rows
      currentFrame$plate[(countLength+1):(countLength+mutantLength)] <- "Selective"
      currentFrame$fraction[(countLength+1):(countLength+mutantLength)] <- 200/(200*1) # P/(CD) = 1
      if(nrow(currentMutants) == 0){
        currentFrame$CFU[countLength+1] <- 0 #
      } else{
        currentFrame$CFU[(countLength+1):(countLength+mutantLength)] <- currentMutants$Count
      }

      # ## Less brain-hurty for-loop method for populating count/selective rows
      # # populate working dataframe
      # for(i in 1:nrow(currentCounts)){
      #   currentFrame$Plate[i] <- "Count"
      #   currentFrame$Fraction[i] <- (600*1E-5)/200
      #   currentFrame$CFU[i] <- currentCounts$mean[i]
      # }
      # for(i in 1:nrow(currentMutants)){
      #   currentFrame$Plate[(i+nrow(currentCounts))] <- "Selective"
      #   currentFrame$Fraction[(i+nrow(currentCounts))] <- (200*1)/200
      #   currentFrame$CFU[(i+nrow(currentCounts))] <- currentMutants$Count[i]
      # }

      # complete the remaining selective rows with 0 CFUs
      currentFrame$plate[(countLength + mutantLength + 1):(countLength + (60 - countPops))] <- "Selective"
      currentFrame$fraction[(countLength + mutantLength + 1):(countLength + (60 - countPops))] <- (200*1)/200
      currentFrame$CFU[(countLength + mutantLength + 1):(countLength + (60 - countPops))] <- 0

      # append currentFrame to master dataframe
      allData <- rbind(allData, currentFrame)
      print(paste("Rep", sheet, "done."))
    }

  }

  dir.create("./Outputs")
  if(isTRUE(dataPooling)){
    write.csv(allData, paste0("./Outputs/", testName, "_pooled.csv"), row.names = FALSE)
    message("\nCompleted. Check Outputs folder for the wrangled .csv file.")
  } else if (isFALSE(dataPooling)){
    write.csv(allData, paste0("./Outputs/", testName, "_unpooled.csv"), row.names = FALSE)
    message("\nCreating a new folder \"Outputs\". Check it for the wrangled .csv file.")
  }

  #read.csv(paste0("./Outputs/", testName, ".csv")

} # end wrangle_fluxdata()
