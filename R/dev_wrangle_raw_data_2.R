# wrangle_raw_data, v2
# for use with the new template

wrangle_raw_data_2 <- function(templateFile, autofill = TRUE, exclude = NULL){
  # ARGS:
  # templateFile - The Excel workbook, each sheet a separate rep; each workbook a separate project.
  # ?

  # What do I need?
  # strain:
  # plate:
  # fraction: countFract now calculated by row
  # CFU:
  # "Well": if used in raw, then parse in wrangler. if no parse in wrangler, then use row/col?
  # Maintain workbook structure (ignore CSV/XLSX option)

  #

  # DEBUG:
  templateFile <- "./dev/data/raws redesign/testbed.xlsx"
  allSheets <- openxlsx::getSheetNames(templateFile)
  sheet <- allSheets[2]

  ###

  # Read in sheets & exclude
  allSheets <- openxlsx::getSheetNames(templateFile)
  sheetNames <- allSheets[!allSheets %in% exclude]

  # Loop over rep sheets and append values to a master df
  allData <- data.frame()
  for(sheet in sheetNames){

    dilution <- 100000

    # get current raw rep and clean up
    currentSheet <- openxlsx::read.xlsx(templateFile, sheet = sheet)
    cols <- c("Name", "Type", "Well", "Volume.taken", "Volume.plated", "Dilution.factor",
              "CFU.observed")
    currentSheet <- currentSheet[, cols]
    currentSheet <- currentSheet[currentSheet$Dilution.factor == dilution, ]

    # split datasets
    currentCounts <- currentSheet[currentSheet$Type == "Count", ]
    currentMutants <- currentSheet[currentSheet$Type == "Selective", ]

    # prep current export df
    currentData <- data.frame(strain = NA, plate = NA, fraction = NA, CFU = NA)

    test <-
      currentCounts |>
      dplyr::group_by(Name, Type, Well, Volume.taken, Volume.plated,
                      Dilution.factor) |>
      #dplyr::mutate(CFU = mean(CFU.observed)) |>
      dplyr::summarize(CFU = mean(CFU.observed)) |>
      dplyr::ungroup()

    currentData$CFU <- test$CFU

    currentCounts$mean <- rowMeans(currentCounts, na.rm = TRUE) # compute count plate means

    # Prep current data
    currentData <- data.frame(strain = rep(sheet, nrow(currentCounts) + (60 - countPops)),
                              plate = NA, fraction = NA, CFU = NA)

    # run the populating function
    currentData <- populate_rows(currentData, currentCounts, currentMutants, countFraction, countPops)

    # append current df to master df and report
    allData <- rbind(allData, currentData)
    cat(paste0("Sheet ", "\"", sheet, "\" ", "done.\n"))

  } # loop exit #



}









