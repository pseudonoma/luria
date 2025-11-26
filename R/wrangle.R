#' Process a luria template file
#'
#' Wrangle a standard fluctuation analysis Excel workbook template, converting it for the pipeline.
#'
#' @details
#' This function converts raw data contained in an Excel workbook template (obtained by running
#' [`get_template()`]) into a standardized CSV file which is required for downstream processing.
#'
#' @section Output data format:
#' The output dataframe has four columns:
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
#' @param exclude.sheets A character vector of sheet names (e.g. replicates) to skip. By default the
#' help sheets *Example layout* and *Column guide* is skipped, but more can be added.
#' @param fill A number indicating the true number of populations (i.e. wells) in each replicate.
#' Additional *Selective* populations will be added with 0 CFU, up to this number. If `NULL`, no
#' filling will occur and only recorded populations will be used.
#' Defaults to `60`, the number of populations in a standard 96-well plate.
#' @param dilution A *Count* population dilution factor to use for wrangling. The same factor will
#' be used for all replicates.
#' Defaults to `1e5`, the standard dilution rate for the *A. baylyi* and *E. coli* protocols.
#' @param export If `TRUE`, outputs will also be exported as a CSV.
#' @param save.as If supplied, the exported file(s) will use this filename. Defaults to `NULL`, in
#' which case the exported file will retain the filename from the input file.
#' @param export.to Where to create the output folder and export the CSV.
#' Defaults to the current directory.
#'
#' @examples
#' wrangle_raw_data(templateFile = "./data/raws/FLUCTEST 1 2020 09 24.xlsx",
#'                  exclude.sheets = c("Rep 0", "Rep 13"),
#'                  save.as = "RIF_Aug2023")
#'
#' @return A dataframe with columns `strain`, `plate`, `fraction`, and `CFU`.
#'
#' @export

wrangle_raw_data <- function(templateFile, exclude.sheets = c("Example layout", "Column guide"),
                             fill = 60, dilution = 1e5,
                             export = FALSE, overwrite = FALSE, save.as = NULL, export.to = "."){

  # ### DEBUG ###
  # templateFile <- dataPath
  # exclude.sheets <- c("Example layout", "Column guide")
  # fill <- 60
  # dilution <- 1e5
  # export <- TRUE
  # overwrite <- FALSE
  # save.as <- NULL
  # export.to <- "."

  # Get the workbook
  sheetList <- openxlsx::getSheetNames(templateFile)
  sheetList <- sheetList[!sheetList %in% exclude.sheets]

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
      dplyr::filter(Dilution.factor == dilution) |>
      dplyr::group_by(Name, Type, Well, Volume.taken, Volume.plated,
                      Dilution.factor) |>
      dplyr::summarize(CFU = mean(CFU.observed)) |>
      dplyr::ungroup()

    # Catch no Count data (most likely due to incorrect dilution provided)
    if(nrow(currentCounts) == 0){
      stop("No valid Count populations after filtering: did you specify the correct dilution?")
    }

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

  # Export as CSV
  if(export){

    # Handle exporting using export helper function
    exportPath <- prep_export(mode = "wrangled", overwrite = overwrite, outputParent = export.to)

    # # Extract default basename & construct exportName
    # baseName <- sub(".xlsx$", "", basename(templateFile))
    # exportName <- paste0(exportPath, "/", baseName)

    # Construct filename
    if(!is.null(save.as)){ # save.as supercedes all other names
      exportName <- paste0(exportPath, "/", save.as)
    } else if(is.null(save.as)){ # no save.as; must parse or construct default
      baseName <- sub(".xlsx$", "", basename(templateFile))
      exportName <- paste0(exportPath, "/", baseName)
      # if(exists(baseName, inherits = FALSE)){
      #   baserName <- sub(".csv", "", baseName)
      #   exportName <- paste0(exportPath, "/", baserName)
      # } else {
      #   timestamp <- paste0(format(Sys.Date(), "%y%m%d"), "-", format(Sys.time(), "%H%M"))
      #   exportName <- paste0(exportPath, "/", timestamp)
      #   warning("Input data has no filename, used current date-time as filename instead.")
      # }
    }

    # Write file and report
    write.csv(exportData, paste0(exportName, ".csv"), row.names = FALSE)
    message("Done. Check luria_output/wrangled/ for the wrangled .csv files.\n")
  }


  return(exportData)

}


#' Create standardized pooled and unpooled data files for the pipeline
#'
#' Prepare CSV files previously wrangled by [`wrangle_raw_data()`] for the rest of the
#' pipeline. Correctly-formatted CSVs otherwise created (e.g. if the Excel template was not
#' used) can also be prepared for the pipeline with this function.
#'
#' @details
#' This function produces two data frames, one with all replicates pooled into one strain, and
#' another with the replicates kept separate. Both CSV files are tidy tables in a standardized
#' format, which is required by [`run_fluxxer()`], the next function in the pipeline.
#'
#' @inheritSection wrangle_raw_data Output data format
#'
#' @inheritParams wrangle_raw_data
#' @inheritParams prep_export
#' @param inputData A CSV file with the standard headers `strain`, `plate`, `fraction`, and `CFU`.
#' Alternatively, a dataframe with those headers.
#' @param pool.as What the pooled strain should be named. If `NULL` (the default), the strain is
#' named *Combined*.
#' @param exclude.reps A character vector of replicates to skip.
#'
#' @examples
#' wrangle_clean_data(inputData = "./data/raws/FLUCTEST 1 2020 09 24.csv",
#'                    pool.as = "AB3",
#'                    exclude.reps = c("Rep 0", "Rep 13"),
#'                    saveAs = "RIF_Aug2023")
#' @return
#' A list of two dataframes named `pooledData` and `unpooledData`, with columns `strain`, `plate`,
#' `fraction`, and `CFU`.
#'
#' @export

wrangle_clean_data <- function(inputData, pool.as = NULL, exclude.reps = NULL,
                               export = FALSE, overwrite = FALSE, save.as = NULL, export.to = "."){

  # ### DEBUG ###
  # inputData <- "./luria_output/wrangled/alicia_test.csv"

  # is the input an object already, or a filename?
  if(is.data.frame(inputData)){
    data <- inputData
  } else if(is.character(inputData)){
    # NTS: CAUTION if file isn't a CSV there's no catcher for it
    data <- read.csv(inputData)
    baseName <- basename(inputData)
  } else {
    stop("input must be either a dataframe object or CSV file.")
  }

  # Check headers and wrangle
  headers <- c("strain", "plate", "fraction", "CFU")
  if(all(headers %in% names(data))){
    # (Crudely) separate data into pooled/unpooled dfs, sans excluded strains
    # NTS: used to be part of subfunction do_wrangle()
    pooledData <- data[!(data$strain %in% exclude.reps), ]
    unpooledData <- data[!(data$strain %in% exclude.reps), ]
    if(is.null(pool.as)){ # optional user-defined name for pooled data
      pooledData$strain <- "Combined"
    } else {
      pooledData$strain <- pool.as
    }
  } else {
    # if headers invalid it mustn't proceed
    stop("The required columns are missing or incorrectly named. Check the file and try again.")
  }

  # Export as CSV
  if(export){

    # Handle export pathing
    exportPath <- prep_export(mode = "wrangled", outputParent = export.to, overwrite)

    # # Construct export filename based on whether basename exists
    # if(exists(baseName, inherits = FALSE)){
    #   # extract basename & construct exportName
    #   baserName <- sub(".csv", "", baseName)
    #   exportName <- paste0(exportPath, "/", baserName)
    # } else if (!exists(baseName, inherits = FALSE)){
    #   timestamp <- paste0(format(Sys.Date(), "%y%m%d"), "-", format(Sys.time(), "%H%M"))
    #   exportName <- paste0(exportPath, "/", timestamp)
    #   warning("Input data has no filename, used current date-time as filename instead.")
    # }

    # Construct filename
    if(!is.null(save.as)){ # save.as supercedes all other names
      exportName <- paste0(exportPath, "/", save.as)
    } else if(is.null(save.as)){ # no save.as; must parse or construct default
      if(exists("baseName", inherits = FALSE)){
        baserName <- sub(".csv", "", baseName)
        exportName <- paste0(exportPath, "/", baserName)
      } else {
        timestamp <- paste0(format(Sys.Date(), "%y%m%d"), "-", format(Sys.time(), "%H%M"))
        exportName <- paste0(exportPath, "/", timestamp)
        warning("Input data has no filename, used current date-time as filename instead.")
      }
    }

    # Export
    write.csv(unpooledData, paste0(exportName, "_unpooled.csv"), row.names = FALSE)
    write.csv(pooledData, paste0(exportName, "_pooled.csv"), row.names = FALSE)

    message("Done. Check luria_output/wrangled/ for the wrangled .csv files.\n")
  }

  # construct return object
  exportObject <- list("pooledData" = pooledData, "unpooledData" = unpooledData)


  return(exportObject)

}


#' Wrangle fluxxer outputs for plotting
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

  # Set mode
  # NTS: internal function, stop adding overwrought error catchers
  if(!is.null(file)){
    runMode <- "single"
  } else if(!is.null(projectName) & !is.null(inputPath)){
    runMode <- "project"
  } else {
    stop("Looks like a bug in mode detection. Please report this.")
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

  } else if(runMode == "project"){

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


#' (Legacy) Convert a fluctuation analysis Excel workbook
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
    # if(is.null(pool.as)){
    #   poolStrain <- "Combined"
    # } else {
    #   poolStrain <- pool.as
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
  message("\nDone. Check luria_output/wrangled/ for the wrangled .csv files.\n")


  return(invisible())

}
