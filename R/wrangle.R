#' Convert a fluctuation analysis Excel workbook.
#'
#' Wrangle a standard fluctuation analysis Excel workbook, converting it for the pipeline.
#'
#' @details
#' This function converts raw data contained in an Excel workbook template into a standardized CSV
#' file which is required by downstream functions. The next function in the pipeline is
#' [`wrangle_clean_data()`].
#'
#' @section Output CSV format:
#' The resulting CSV is a tidy table with four columns:
#' \itemize{
#'    \item `strain` - The name of the replicate
#'    \item `plate` - The type of plate, either *Count* or *Selective*
#'    \item `fraction` - The fraction of culture plated (see below)
#'    \item `CFU` - The observed number of colonies on the plate.
#' }

#' Additionally, fraction is given as \eqn{F = P\div(CD)}, where P = volume plated, in uL;
#' C = volume of Count culture used for dilution, in uL; and D = dilution factor. Note that D is
#' expressed as an integer (eg. \eqn{10^{3}}) and not a ratio (eg. \eqn{10^{-3}}). Fraction is
#' calculated individually for every observation in the raw file.
#'
#' @import openxlsx dplyr
#'
#' @inheritParams prep_export
#' @param templateFile The filename of the standard Excel workbook.
#' @param exclude A character vector of sheet names (e.g. replicates) to skip. By default the
#' help sheets *Example layout* and *Column guide* is skipped, but more can be added.
#' @param fill A number indicating the true number of populations (i.e. wells) in each replicate.
#' Additional *Selective* populations will be added with 0 CFU, up to this number. If `NULL`, no
#' filling will occur and only recorded populations will be used.
#' Defaults to `60`, the number of populations in a standard 96-well plate.
#' @param dilution A *Count* population dilution factor to use for wrangling. The same factor will
#' be used for all replicates.
#' Defaults to `1e5`, the standard dilution rate for the *A. baylyi* and *E. coli* protocols.
#' @param saveAs A filename for saving the wrangled file.
#' Defaults to `NULL`, in which case the original filename will be used.
#'
#' @examples
#' wrangle_raw_data(templateFile = "./data/raws/FLUCTEST 1 2020 09 24.xlsx",
#'                  exclude = c("Rep 0", "Rep 13"),
#'                  saveAs = "RIF_Aug2023")
#'
#' @export

wrangle_raw_data <- function(templateFile, exclude = c("Example layout", "Column guide"),
                             fill = 60, dilution = 1e5, saveAs = NULL, overwrite = FALSE){

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
    baseName <- sub(".xlsx$", "", basename(templateFile))
    exportName <- paste0(exportPath, "/", baseName)
  } else {
    # or construct using <saveAs> value
    exportName <- paste0(exportPath, "/", saveAs)
  }

  # Write file and report
  write.csv(exportData, paste0(exportName, ".csv"), row.names = FALSE)
  message("\nDone. Check output/wrangled/ for the wrangled .csv files.\n")


  return(invisible())

}


#' Create standardized pooled and unpooled data files for the pipeline.
#'
#' Prepare CSV files previously wrangled by [`wrangle_raw_data()`] for the rest of the
#' pipeline. Correctly-formatted CSVs otherwise created (e.g. if the Excel template was not
#' used) can also be prepared for the pipeline with this function.
#'
#' @details
#' This function produces two CSV files, one with all replicates pooled into one strain, and another
#' CSV with the replicates kept separate. Both CSV files are tidy tables in a standardized format,
#' which is required by [`run_fluxxer()`], the next function in the pipeline.
#'
#' @inheritSection wrangle_raw_data Output CSV format
#'
#' @inheritParams wrangle_raw_data
#' @inheritParams prep_export
#' @param dataFile A CSV file with the standard headers `strain`, `plate`, `fraction`, and `CFU`.
#' @param poolAs What the pooled strain should be named.
#' Defaults to `NULL`, in which case the strain is just named *Combined*.
#' @param exclude A character vector of replicates to skip.
#'
#' @examples
#' wrangle_clean_data(dataFile = "./data/raws/FLUCTEST 1 2020 09 24.csv",
#'                    poolAs = "AB3",
#'                    exclude = c("Rep 0", "Rep 13"),
#'                    saveAs = "RIF_Aug2023")
#' @return
#' Two tidy CSVs with columns `strain`, `plate`, `fraction`, and `CFU`.
#'
#' @export

wrangle_clean_data <- function(dataFile = NULL, poolAs = NULL, exclude = NULL,
                               saveAs = NULL, overwrite = FALSE){

  # Explicitly set mode
  runMode <- ifelse(is.null(dataFile), yes = "standard", no = "single")

  # Handle export pathing
  exportPath <- prep_export(mode = "wrangled", overwrite)

  # Define standard header for checks
  headers <- c("strain", "plate", "fraction", "CFU")

  ### Define wrangle-export subroutine #####
  do_wrangle <- function(data, baseName, poolAs, exclude, saveAs){

    # Crudely read in pooled/unpooled data, sans excluded strains
    pooledData <- data[!(data$strain %in% exclude), ]
    unpooledData <- data[!(data$strain %in% exclude), ]
    if(is.null(poolAs)){
      pooledData$strain <- "Combined"
    } else {
      pooledData$strain <- poolAs
    }

    # Construct export filename
    if(is.null(saveAs)){
      # extract basename & construct exportName
      baserName <- sub(".csv", "", baseName)
      exportName <- paste0(exportPath, "/", baserName)
    } else {
      # or construct using <saveAs> value
      exportName <- paste0(exportPath, "/", saveAs)
    }

    # Export
    write.csv(unpooledData, paste0(exportName, "_unpooled.csv"), row.names = FALSE)
    write.csv(pooledData, paste0(exportName, "_pooled.csv"), row.names = FALSE)

  } ### end do_wrangle() #####

  # Begin wrangle

  ##### Single-file mode #####
  if(runMode == "single"){

    # Read in file
    data <- read.csv(dataFile)
    baseName <- basename(dataFile)

    # If headers are valid, wrangle & export
    if(all(headers %in% names(data))){
      do_wrangle(data, baseName, poolAs, exclude, saveAs)
    } else {
      # headers appear invalid
      stop("The required columns are missing or incorrectly named. Check your file and try again.")
    }

    ##### Standard pipeline mode #####
  } else if(runMode == "standard"){

    # Get CSVs from the standard input location...
    inputPath <- "./output/wrangled"
    csvList <- grep(".csv$", dir(inputPath, full.names = TRUE), value = TRUE)
    # ...and grab only CSVs which don't look like pipeline files (in case overwrite = T)
    pooledList <- grep("_pooled.csv$", dir(inputPath, full.names = TRUE), value = TRUE)
    unpooledList <- grep("_unpooled.csv$", dir(inputPath, full.names = TRUE), value = TRUE)
    validList <- csvList[which(!(csvList %in% pooledList) & !(csvList %in% unpooledList))]

    # Catch no valid files
    if(length(validList) == 0){
      stop("No valid files to process. Note that pooled/unpooled file pairs are already processed.")
    }

    # Loop over valid, non-pipeline CSVs
    for(file in validList){

      # Read in files just like single-mode
      currentData <- read.csv(file)
      baseName <- basename(file)

      # If headers are valid, wrangle & export
      if(all(headers %in% names(currentData))){
        message(paste0("Processing ", baseName))
        do_wrangle(currentData, baseName, poolAs, exclude, saveAs)
      }
    } # (no error message here because it's not the user's job to validate inputs?)

  }

  message("\nDone. Check output/wrangled/ for the wrangled .csv files.\n")


  return(invisible())

}


#' Wrangle fluxxer outputs for plotting.
#'
#' Internal function called by [`plot_fluxxer()`]. It imports and wrangles analyzed data to prepare
#' it for plotting.
#'
#' @details
#' This is a single/standard mode function, taking either an explicit fluxxer .output.csv file, or
#' automatically retrieving these files from /output/analyzed/. It (a) produces a data object of
#' combined pooled and unpooled data; (b) calls [`refactor_reps()`] to rearrange replicate names for
#' plotting; and (c) calls [`test_logticks()`] to detect if the data will cause
#' [`ggplot2::annotation_logticks`] to throw an error due to error bars not spanning 2 logticks.
#'
#' @param file The filename of the analyzed .output.csv to wrangle. If supplied, the function will
#' run in single file mode.
#' @param projectName A project prefix as understood by the pipeline. If supplied, the function will
#' run in standard pipeline mode.
#' @param inputPath The path for the pipeline output folder from which files are automatically
#' retrieved. This must be supplied by [`plot_fluxxer()`] if `projectName` is supplied.
#'
#' @examples
#' wrangle_plot_data(file = "./data/old_fluc_stuff/FLUCTEST1.output.csv",
#'                   projectName = NULL,
#'                   inputPath = NULL)
#'
#' @return A list with: `data`, the plotting data; `levels`, a vector of replicate names in the
#' correct order; and `log`, which if `TRUE` will signal [`plot_mutrates()`] to use logticks.
#'
#' @keywords internal

wrangle_plot_data <- function(file = NULL, projectName = NULL, inputPath = NULL){

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


#' (Legacy) Convert a fluctuation analysis Excel workbook.
#'
#' Wrangle a legacy-format standard fluctuation analysis Excel workbook, converting it for the
#' pipeline.
#'
#' @details
#' This function works on the old Excel workbook format to produce the standard CSV file with
#' columns `strain`, `plate`, `fraction`, and `CFU`. Templates obtained by calling
#' [`get_template()`] must be wrangled with [`wrangle_raw_data()`] instead. The next function in
#' the pipeline is [`wrangle_clean_data()`].
#'
#' @import openxlsx
#'
#' @inheritParams prep_export
#' @param dataFile The filename of the standard Excel workbook.
#' @param countPops An integer value indicating how many Count populations were used in each test.
#' This should be one value for all tests, and it should not account for failed Count plates.
#' @param countFract A named vector for calculating the fraction plated, F, of the Count
#' populations. Fraction is given as \eqn{F = P\div(CD)}, where P = volume plated, in uL;
#' C = volume of Count culture used for dilution, in uL; and D = dilution factor, expressed as an
#' integer (eg. \eqn{10^{3}}) and **not** a ratio (eg. \eqn{10^{-3}}). The same vector is applied
#' to all tests. The default values are based on the original *A. baylyi* protocol.
#' @param exclude A character vector of sheet names (i.e. replicates) to skip. By default the sheet
#' "Summary" is skipped, but more can be added.
#' @param saveAs A filename for saving the wrangled file.
#' Defaults to `NULL`, in which case the original filename will be used.
#'
#' @examples
#' wrangle_old_raws(dataFile = "./data/raws/FLUCTEST 1 2020 09 24.xlsx",
#'                  countPops = 4,
#'                  exclude = c("Summary", "Rep 0", "Rep 13"),
#'                  saveAs = "RIF_Aug2023")
#'
#' @export

wrangle_old_raws <- function(dataFile, countPops, countFract = c(P = 200, C = 200, D = 1E5),
                             exclude = "Summary", saveAs = NULL, overwrite = FALSE){

  # Prep some variables
  allSheets <- openxlsx::getSheetNames(dataFile)
  sheetNames <- allSheets[!allSheets %in% exclude] # only unexcluded sheet names
  countFraction <- countFract["P"]/(countFract["C"] * countFract["D"])
  if(is.na(countFraction)){
    stop("Couldn't calculate Count plating fraction; check that countFract is a named vector.")
  }

  # Make overall dataframe object
  allData <- data.frame()

  # Loop over sheets and fill the master dfs
  for(sheet in sheetNames){

    # extract the appropriate sheets sheets and slice out the chunks
    currentCounts <- openxlsx::read.xlsx(dataFile, sheet = sheet,
                                         rows = 3:(nrow(openxlsx::read.xlsx(dataFile, sheet = sheet)) + 1),
                                         cols = 2:4) # ignore 10E-6 counts
    currentMutants <- openxlsx::read.xlsx(dataFile, sheet = sheet,
                                          rows = 2:(nrow(openxlsx::read.xlsx(dataFile, sheet = sheet)) + 1),
                                          cols = 7:8)
    currentCounts$mean <- rowMeans(currentCounts, na.rm = TRUE) # compute count plate means

    # # prep the two streams of current dfs
    # if(is.null(poolAs)){
    #   poolStrain <- "Combined"
    # } else {
    #   poolStrain <- poolAs
    # }
    # unpooledData <- data.frame(strain = rep(sheet, nrow(currentCounts) + (60 - countPops)),
    #                            plate = NA, fraction = NA, CFU = NA)
    # pooledData <- data.frame(strain = rep(poolStrain, nrow(currentCounts) + (60 - countPops)),
    #                          plate = NA, fraction = NA, CFU = NA)

    # Prep current data
    currentData <- data.frame(strain = rep(sheet, nrow(currentCounts) + (60 - countPops)),
                              plate = NA, fraction = NA, CFU = NA)

    # run the populating function
    currentData <- populate_rows(currentData, currentCounts, currentMutants, countFraction, countPops)

    # append current df to master df and report
    allData <- rbind(allData, currentData)
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
  write.csv(allData, paste0(exportName, ".csv"), row.names = FALSE)
  message("\nDone. Check output/wrangled/ for the wrangled .csv files.\n")


  return(invisible())

}
