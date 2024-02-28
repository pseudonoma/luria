# wrangle_raw_data, v2
# for use with the new template

wrangle_raw_data_2 <- function(templateFile, exclude = NULL, fill = 60, dilution = 1e5){
  # ARGS:
  # templateFile - The excel file (probably change this since it's not always a template)
  # fill - Total number of "wells" to fill to; if NULL, does not fill
  # exclude -
  # dilution - what dilution to use for the Count populations, because it's not possible to
  #                  aggregate over multiple dilutions without some kind of internal averaging

  # DEBUG
  templateFile <- "./dev/data/raws redesign/testbed.xlsx"
  exclude <- c("allData clean") #, "STP Rep16"
  dilution <- 1e5
  fill <- NULL
  sheet <- sheetList[4]

  ###

  # Get the workbook
  sheetList <- openxlsx::getSheetNames(templateFile)
  sheetList <- sheetList[!sheetList %in% exclude]

  # Make overall dataframe object
  exportData <- data.frame()

  # Loop over sheets and fill the master dfs
  for(sheet in sheetList){

    # Get current rep and clean up
    currentSheet <- openxlsx::read.xlsx(templateFile, sheet)
    currentSheet <- currentSheet[, c("Name", "Type", "Well", "Volume.taken", "Volume.plated",
                                     "Dilution.factor", "CFU.observed")]

    # Prep datasets
    currentCounts <- currentSheet[currentSheet$Type == "Count" &
                                    !is.na(currentSheet$CFU.observed), ]
    currentMutants <- currentSheet[currentSheet$Type == "Selective" &
                                     !is.na(currentSheet$CFU.observed), ]

    # Summarize Count data and collapse CFUs
    currentCounts <-
      currentCounts |>
      filter(Dilution.factor == dilution) |>
      dplyr::group_by(Name, Type, Well, Volume.taken, Volume.plated,
                      Dilution.factor) |>
      dplyr::summarize(CFU = mean(CFU.observed)) |>
      dplyr::ungroup()

    # Handle autofill and current df rowcount
    countLength <- nrow(currentCounts)
    mutantLength <- nrow(currentMutants)
    if(!is.null(fill)){ # fill to standard plate capacity
      rowTotal <- fill
    } else if(mutantLength >= 1){ # do not fill & >=1 mutant has CFUs
      rowTotal <- countLength + mutantLength
    } else if(mutantLength == 0){ # this might need to be put back in later #####
      warning(paste("Skipped", sheet, "because there are no mutants."))
      next
    }
    if(length(unique(currentCounts$Well)) != countLength){
      stop("# of Count observations != # of unique Count wells reported. This might be a problem.")
    }

    # Prepare current df
    strain <- unique(currentSheet$Name)
    if(length(strain) > 1){
      stop("Multiple replicate names detected in a single replicate dataset.")
    }
    currentData <- data.frame(strain = rep(strain, rowTotal), plate = NA, fraction = NA, CFU = NA)

    ##### Begin fill #####

    # 1. Fill Count rows
    for(i in 1:countLength){
      currentData$plate[i] <- "Count"
      currentData$fraction[i] <- currentCounts$Volume.plated[i]/(currentCounts$Volume.taken[i] *
                                                                   currentCounts$Dilution.factor[i])
      currentData$CFU[i] <- currentCounts$CFU[i]
    }

    # 2. Fill Selective rows with values
    mutantStart <- countLength + 1
    mutantEnd <- countLength + mutantLength
    currentData$plate[mutantStart:(countLength + mutantLength)] <- "Selective"
    currentData$fraction[mutantStart:(countLength + mutantLength)] <-
      currentMutants$Volume.plated/(currentMutants$Volume.taken * currentMutants$Dilution.factor)
    currentData$CFU[mutantStart:(countLength + mutantLength)] <- currentMutants$CFU.observed

    # 3. Fill 0 CFU rows if fill arg is active
    if(rowTotal > countLength + mutantLength){ # TRUE would imply fill was active
      fillStart <- mutantEnd + 1
      currentData$plate[fillStart:rowTotal] <- "Selective"
      currentData$fraction[fillStart:rowTotal] <-  mean(currentData$fraction[currentData$plate == "Selective"],
                                                        na.rm = TRUE)
      currentData$CFU[fillStart:rowTotal] <- 0
    }

    ##### End fill #####

    # Append current df to master df and report
    exportData <- rbind(exportData, currentData)
    cat(paste0("Dataset ", "\"", sheet, "\" ", "done.\n"))

  }

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
  write.csv(allData, paste0(exportName, ".csv"), row.names = FALSE)
  message("\nDone. Check output/wrangled/ for the wrangled .csv files.\n")


  return(invisible())

}

