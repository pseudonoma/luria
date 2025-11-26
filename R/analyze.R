#' Run the luria pipeline
#'
#' Automatically process one or more template files to estimate mutation rates, export them, plot
#' the results, and (optionally) export the mutation rates.
#'
#' @details
#' A wrapper function that runs [`wrangle_raw_data()`], [`wrangle_clean_data()`], [`run_fluxxer()`],
#' and [`plot_fluxxer()`] on a given template file or a folder containing multiple template files.
#' Argument values are inherited from those functions and can be specified in the same way.
#' `extract.mutrates` can optionally be set to `TRUE` to run [`extract_mutrates()`], which will
#' return and/or export the mutation rate numbers for other uses.
#'
#' If a folder of multiple template files is supplied to `templateData`, values to arguments such as
#' `exclude.sheets` are applied to all of them. To specify different argument values for multiple
#' template files, run this function on each template file separately with `overwrite = TRUE`.
#'
#' @inheritParams wrangle_raw_data
#' @inheritParams wrangle_clean_data
#' @inheritParams run_fluxxer
#' @inheritParams prep_export
#' @param templateData A luria Excel workbook template file, or a folder containing such.
#' @param export If `TRUE` (the default), outputs will be saved to a folder `\luria_output\` in the
#' current directory.
#' @param extract.mutrates If `TRUE`, `extract_mutrates()` is also run. Defaults to `FALSE`.
#'
#' @examples
#' auto_luria(templateData, exclude.sheets = c("Example layout", "Column guide"),
#'            fill = 60, dilution = 1e5,
#'            pool.as = NULL, exclude.reps = NULL, extract.mutrates = FALSE,
#'            export = TRUE, overwrite = FALSE)
#'
#' @return A list object containing the outputs of the component functions. If a folder is supplied
#' to `templateData`, each raw file will produce a list of list objects corresponding to that raw
#' file.
#'
#' @export

auto_luria <- function(templateData, exclude.sheets = c("Example layout", "Column guide"),
                       fill = 60, dilution = 1e5,
                       pool.as = NULL, exclude.reps = NULL, extract.mutrates = FALSE,
                       export = TRUE, overwrite = FALSE){

  # Define core pipeline function
  run_luria_pipeline <- function(templateData, exclude.sheets, fill, dilution,
                                 pool.as, exclude.reps,
                                 export, overwrite, export.to){

    # Get project name to pass to save.as arguments
    projName <- sub(".csv", "", basename(templateData))
    # Begin running pipeline
    pipelineOutput <- list() # list of all output dataframes and plot object
    message("Wrangling raw data...")
    cleanData <- wrangle_raw_data(templateFile = templateData,
                                  exclude.sheets = exclude.sheets,
                                  fill = fill,
                                  dilution = dilution,
                                  export = export,
                                  overwrite = overwrite,
                                  save.as = projName,
                                  export.to = export.to)
    message("Wrangling clean data...")
    fluxxerData <- wrangle_clean_data(inputData = cleanData,
                                      pool.as = pool.as,
                                      exclude.reps = exclude.reps,
                                      export = export,
                                      overwrite = overwrite,
                                      save.as = projName,
                                      export.to = export.to)
    message("Running fluxxer...")
    run_fluxxer(inputData = fluxxerData,
                comparisons = TRUE, # forced
                overwrite = overwrite,
                save.as = projName,
                export.to = export.to)
    outputList <- list("cleanData" = cleanData, "fluxxerInput" = fluxxerData)
    # Data pipeline disconnects here; plot_fluxxer...
    #   retrieves files from hardcoded path, but the wrapper enforces paths
    #   automatically gets projName from dataPath
    message("Plotting...")
    plots <- plot_fluxxer(dataPath = "./luria_output/analyzed", export = TRUE, overwrite)
    outputList[["plots"]] <- plots
    # Construct return object
    pipelineOutput <- list("data" = outputList, "projName" = projName)

    return(pipelineOutput)

  } # END run_luria_pipeline #

  # Detect mode
  if(is.character(templateData)){
    if(utils::file_test("-d", templateData)){
      runMode <- "multi"
    } else {
      runMode <- "single"
    }
  }

  # Run pipeline depending on inputs
  # NTS:
  # Topmost /output/ dir announced/created by prep_export() on first run_luria_pipeline call
  # export.to is explicitly default; hardcoded paths in wrapper so user must not specify
  # run_luria_pipeline returns (a) list of dfs, (b) projName

  # Single raw file
  if(runMode == "single"){

    # Run pipeline
    currentOutput <- run_luria_pipeline(templateData, exclude.sheets, fill, dilution,
                                        pool.as, exclude.reps,
                                        export, overwrite, export.to = ".")
    returnObject <- currentOutput$data

  } else if(runMode == "multi"){

    allOutputs <- list()
    for(rawfile in dir){

      message(paste0("Working on ", rawfile, " ..."))

      # loop over each rawfile, assigning outputs to a containing list object
      currentOutput <- run_luria_pipeline(templateData, exclude.sheets, fill, dilution,
                                          pool.as, exclude.reps,
                                          export, overwrite, export.to = ".")
      allOutputs[[currentOutput$projName]] <- currentOutput$data
    }

    # Return object is list of project names, each one a list of its output data
    returnObject <- allOutputs
  }

  # Must be after main pipeline completes (I think) or it'll re-extract after every rawfile
  if(extract.mutrates){
    dataPath <- "./luria_output/analyzed"
    mutrateData <- extract_mutrates(dataPath, export.method = "all", overwrite)
    returnObject[["mutrate"]] <- mutrateData
  }

  message("\nDid you know? The banana is a berry but the strawberry isn't.")
  message("Anyway, the luria pipeline ran successfully, congratulations!\n")


  return(returnObject)

}

#' Analyze the data and estimate mutation rates
#'
#' Estimate mutation rates using the wrangled fluctuation analysis data produced by
#' [`wrangle_clean_data()`] and export the results.
#'
#' @details
#' This function calls [`calculate_mut_rate()`] to estimate mutation rates. It can be supplied the
#' list object produced by [`wrangle_clean_data()`], a directory containing pooled/unpooled CSV file
#' pairs, or else a single dataframe or file produced by the same method. Every file or dataframe
#' will produce a corresponding output file named `*.output.csv` saved to `/luria_output/analyzed/`.
#'
#' If `comparisons = TRUE`, an additional file named `*.comparisons.csv` will be generated; this
#' file contains pairwise comparisons between all replicates and p-values indicating how different
#' the mutation rates are from each other. Where pooled/unpooled data pairs are supplied, comparison
#' files are generated from unpooled data. Note that supplying a single dataframe or file with
#' `comparisons = TRUE` will generate a comparisons file even if the data has only one strain.
#'
#' @inheritParams prep_export
#' @inheritParams wrangle_raw_data
#' @param inputData A correctly-formatted dataframe, or a list containing a pair of such produced
#' by [`wrangle_clean_data()`], to estimate mutation rates from. Alternatively, a well-formed CSV
#' file, or a folder containing pairs of pooled/unpooled data CSVs.
#' @param comparisons Logical value indicating if a comparison file should be generated.
#'
#' @examples
#' run_fluxxer(comparisons = TRUE)
#'
#' @export

run_fluxxer <- function(inputData, comparisons = TRUE,
                        overwrite = FALSE, save.as = NULL, export.to = "."){

  # ### DEBUG ###
  # inputData <- "./luria_output/wrangled/alicia_test_unpooled.csv"
  # comparisons <- TRUE
  # overwrite <- FALSE
  # save.as <- NULL
  # export.to <- "."
  # outputPath <- "./luria_output/analyzed"

  # Detect input type
  if(is.character(inputData)){
    inputIsPath <- TRUE
  } else {
    inputIsPath <- FALSE
  }

  # NTS: Exporting is mandatory so must define these now
  # Create standard output folders
  outputPath <- prep_export(mode = "analyzed", outputParent = export.to, overwrite = overwrite)

  # Function to get/make filenames for exporting, if possible
  # Slightly different version than in the wrangle_ funcs: doesn't use exportPath
  # NTS: exportName is everything up to .csv, so eg. _unpooled is also included
  make_exportname <- function(fileName = NULL, save.as = save.as){
    # main function passes NULL to save.as if it's not specified there
    if(!is.null(save.as)){
      exportName <- save.as
    } else if (is.null(save.as)){
      if(!is.null(fileName)){
        exportName <- sub(".csv", "", fileName)
      } else {
        exportName <- paste0(format(Sys.Date(), "%y%m%d"), "-", format(Sys.time(), "%H%M"))
        warnFlag <- TRUE
        # warning("Input data has no filename, used current date-time as filename instead.")
      }
    }
    return(exportName)
  }

  # Set comparisons conditions crudely so only unpooled data is used
  if(comparisons){
    pooledCompare <- FALSE
    unpooledCompare <- TRUE
  } else {
    pooledCompare <- FALSE
    unpooledCompare <- FALSE
  }

  # Begin wrangle according to input type
  warnFlag <- FALSE # will trigger if exportName defaults to timestamp for any data

  if(inputIsPath){ # (Option A - inputData is file/dir)

    if(utils::file_test("-d", inputData)){ # is folder containing pooled/unpooled filepair
      # Look for the files
      inputPath <- inputData # just for my own clarity
      message(paste0("Looking in", inputPath, "for pooled/unpooled file pairs."))
      if(!dir.exists(inputPath)){
        stop("Folder not found.")
      }

      # Get pooled/unpooled filenames
      pooledList <- grep("_pooled", dir(inputPath), value = TRUE)
      unpooledList <- grep("_unpooled", dir(inputPath), value = TRUE)

      # Run core fluxxer function on pooled, then unpooled, data
      for(fileName in pooledList){
        exportName <- make_exportname(fileName, save.as)
        calculate_mut_rate(inputData = paste0(inputPath, "/", fileName),
                           outputPath,
                           outputPrefix = exportName, # filepair inputs will have _unpooled in name
                           comparisons = pooledCompare)
      }
      for(fileName in unpooledList){
        exportName <- make_exportname(fileName, save.as)
        calculate_mut_rate(inputData = paste0(inputPath, "/", fileName),
                           outputPath,
                           outputPrefix = exportName,
                           comparisons = unpooledCompare)
      }

    } else { # is single file
      # NTS: Assumes if inputData is string and not folder, it's a file
      exportName <- make_exportname(fileName = basename(inputData), save.as)
      calculate_mut_rate(inputData, # arg should be single filepath string
                         outputPath,
                         outputPrefix = exportName,
                         comparisons = comparisons)
    }
  } else if(!inputIsPath){ # (Option B - inputData is object)

    if(is.data.frame(inputData)){ # user-supplied single-dataframe
      # run calculate_mut_rate on the single df
      exportName <- make_exportname(fileName = NULL, save.as)
      calculate_mut_rate(inputData,
                         outputPath,
                         outputPrefix = exportName, # NO BASENAME AVAILABLE
                         comparisons = comparisons)

    } else if(is.list(inputData)){ # expected pipeline object from wrangle_clean
      # run on each dataframe separately
      exportName <- make_exportname(fileName = NULL, save.as)
      calculate_mut_rate(inputData = inputData$pooledData,
                         outputPath,
                         outputPrefix = paste0(exportName, "_pooled"),
                         comparisons = pooledCompare)
      calculate_mut_rate(inputData = inputData$unpooledData,
                         outputPath,
                         outputPrefix = paste0(exportName, "_unpooled"),
                         comparisons = unpooledCompare)
    }

  }

  message("Done. Check /analyzed/ for outputs.\n")
  if(warnFlag){
    warning("Couldn't get filenames for some data, used current date-time as filename instead.")
  }


  return(invisible()) # NTS: intentional, outputs must be exports so nothing to return

}


#' Calculate mutation rates
#'
#' A minimally-modified version of `calculateMutRate()` from the original
#' [`fluxxer.R`](https://github.com/barricklab/barricklab/blob/master/fluxxer.R) script. Do not call
#' this function manually; use [`run_fluxxer()`] instead.
#'
#' @details
#' This function uses the rSalvador package to obtain the maximum likelihood estimates for
#' mutation rates based on the Luria-Delbruck distribution. This version does not produce plots, as
#' it is handled by the pipeline function [`plot_fluxxer()`].
#'
#' @import rsalvador dplyr tibble readr
#'
#' @param inputData Input data. If a filename, must be a correctly-formatted CSV to estimate
#' mutation rates from.
#' @param outputPath The directory to save output files to.
#' @param outputPrefix A prefix to use when saving the output file, which will be named like
#' `<outputPrefix>.output.csv`
#' @param comparisons Logical. Indicates if a comparison file should be generated.
#'
#' @export

calculate_mut_rate <- function(inputData,
                               outputPath,
                               outputPrefix = "",
                               comparisons = FALSE){

  # ### DEBUG ###
  # inputData <- inputData$pooledData
  # outputPath <- outputPath
  # outputPrefix <- paste0(exportName, "_pooled")
  # comparisons <- pooledCompare

  # require libraries:
  # suppressMessages(library(rsalvador))
  # suppressMessages(library(tidyverse))
  # suppressMessages(library(cowplot))
  # suppressMessages(library(optparse))

  # if ((output_prefix!= "") && !grepl('[./]$', output_prefix)) {
  #   output_prefix = paste0(output_prefix, ".")
  # }
  #
  # if (is.null(filename)) {
  #   stop("No valid file supplied.")
  # }

  #read in file specified. Must be in same directory
  #for testing
  #data <- read_csv("example_dataset_2.csv")

  # read in data depending on input type
  if(is.data.frame(inputData)){
    data <- inputData
  } else if(is.character(inputData)){
    data = readr::read_csv(inputData)
  }

  #do some checks of the input files to expand abbreviations
  data$plate = tolower(data$plate)
  data = dplyr::mutate(data, plate = ifelse( (plate == "n") | (plate == "ns") | (plate == "count"), "nonselective", plate))
  data = dplyr::mutate(data, plate = ifelse(plate == "s", "selective", plate))

  data$strain = as.factor(data$strain)
  data$plate = as.factor(data$plate)

  strains = levels(data$strain)

  #identify # of strains, use to build empty data frame
  num_strains <- length(strains)
  cat("Found", num_strains, "strains:\n")
  cat(strains, sep='\n')

  output_data <- tibble::tibble()

  #cycle through each column to calculate mutatation rate and confidence
  for(this.strain in strains) {
    cat("\nSTRAIN:", this.strain, "\n")
    #locate Non_selective separator
    this.strain.data = data %>% filter(strain==this.strain)

    #extract selective values
    selective.rows = this.strain.data %>% dplyr::filter(plate=="selective")
    nonselective.rows = this.strain.data %>% dplyr::filter(plate=="nonselective")
    num_selective = nrow(selective.rows)
    num_nonselective = nrow(nonselective.rows)

    cat("Number of selective plate counts:", num_selective, "\n")
    cat("Number of nonselective plate counts:", num_nonselective, "\n")

    if (num_selective == 0 || num_nonselective == 0 ) {
      cat("***ERROR! Did not find plate counts for selective/nonselective. Skipping strain.\n")
      next
    }

    nonselective_cell_counts = mean(nonselective.rows$CFU/nonselective.rows$fraction)
    cat("Estimated cells per culture:", nonselective_cell_counts, "(", nrow(nonselective.rows), "nonselective plates )\n")

    #all selective plates must have the same fraction
    selective_fraction_list = selective.rows %>% dplyr::count(fraction)
    if (nrow(selective_fraction_list) > 1) {
      cat("***ERROR! Multiple fractions found for selective plates. Skipping strain.\n")
      next
    }
    selective_fraction = selective_fraction_list$fraction[1]
    cat("Fraction or efficiency of selective cultures plated (e):", selective_fraction, "\n")

    if (selective_fraction == 1) {
      m = rsalvador::newton.LD(selective.rows$CFU)
    } else {
      m = rsalvador::newton.LD.plating(selective.rows$CFU, e=selective_fraction)
    }

    mu = m / nonselective_cell_counts
    cat("Maximum likelihood mutation rate (mu):", mu, "\n")

    if (selective_fraction == 1) {
      CI = rsalvador::confint.LD(selective.rows$CFU, alpha=0.05)/nonselective_cell_counts
    } else {
      CI = rsalvador::confint.LD.plating(selective.rows$CFU, alpha=0.05, e=selective_fraction)/nonselective_cell_counts
    }
    cat("         95% confidence interval (mu): [", CI[1], ",", CI[2] , "]\n")

    output_data = rbind(output_data, data.frame(strain = this.strain,
                                                num_nonselective_plates = num_nonselective,
                                                num_selective_plates = num_selective,
                                                selective_fraction = selective_fraction,
                                                avg_cells_per_culture = nonselective_cell_counts,
                                                mu = mu,
                                                CI.95.lower = CI[1],
                                                CI.95.higher = CI[2]))
  }

  # export main output data
  readr::write_csv(output_data, paste0(outputPath, "/", outputPrefix, ".output.csv"))

  # ##make chart for pretty values
  # plot <- ggplot(output_data, aes(x = strain, y = mu)) +
  #   geom_point() +
  #   geom_linerange(aes(ymin = CI.95.lower, ymax = CI.95.higher)) +
  #   scale_y_log10() +
  #   ggtitle("Mutation Rates") +
  #   xlab("Strains") +
  #   ylab("Mutation rate MLE") +
  #   annotation_logticks(sides = "l")+
  #   theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  #
  # save_plot(paste0(output_prefix, "plot.pdf"), plot)

  # Optional code that performs comparisons between rates
  if (comparisons) {
    comparison_data = data.frame()

    for(i in 1:length(strains)) {
      j = i
      while(T) {
        j = j + 1
        if (j > length(strains)) {
          break
        }
        cat("\nComparing mutation rates...", "\n")
        cat("  STRAIN 1:", strains[i], "\n")
        cat("  STRAIN 2:", strains[j], "\n")

        this.strain.i = strains[i]
        this.strain.j = strains[j]

        this.strain.data.i = data %>% dplyr::filter(strain==this.strain.i)
        this.strain.data.j = data %>% dplyr::filter(strain==this.strain.j)

        selective.rows.i = this.strain.data.i %>% dplyr::filter(plate=="selective")
        selective.rows.j = this.strain.data.j %>% dplyr::filter(plate=="selective")

        nonselective.rows.i = this.strain.data.i %>% dplyr::filter(plate=="nonselective")
        nonselective.rows.j = this.strain.data.j %>% dplyr::filter(plate=="nonselective")

        if (nrow(selective.rows.i) == 0 || nrow(nonselective.rows.i) == 0 ) {
          cat("***ERROR! Did not find plate counts for selective/nonselective. Skipping pair\n")
          next
        }

        if (nrow(selective.rows.i) == 0 || nrow(nonselective.rows.j) == 0 ) {
          cat("***ERROR! Did not find plate counts for selective/nonselective. Skipping pair\n")
          next
        }

        #all selective plates must have the same fraction
        selective_fraction_list.i = selective.rows.i %>% dplyr::count(fraction)
        if (nrow(selective_fraction_list.i) > 1) {
          cat("***ERROR! Multiple fractions found for selective plates. Skipping pair\n")
          next
        }
        selective_fraction.i = selective_fraction_list.i$fraction[1]

        selective_fraction_list.j = selective.rows.j %>% dplyr::count(fraction)
        if (nrow(selective_fraction_list.j) > 1) {
          cat("***ERROR! Multiple fractions found for selective plates. Skipping pair\n")
          next
        }
        selective_fraction.j = selective_fraction_list.j$fraction[1]

        nonselective_cell_counts.i = mean(nonselective.rows.i$CFU/nonselective.rows.i$fraction)
        nonselective_cell_counts.j = mean(nonselective.rows.j$CFU/nonselective.rows.j$fraction)

        cat("  R:", nonselective_cell_counts.j/nonselective_cell_counts.i, "\n")
        cat("  e1:", selective_fraction.i, "\n")
        cat("  e2:", selective_fraction.j, "\n")

        # Use simpler rSalvador function with plating efficiencies are 100%
        # because it is more robust to failures...
        this.result = c()
        if ((selective_fraction.i==1) & (selective_fraction.j==1)) {
          this.result = rsalvador::LRT.MK(
            selective.rows.i$CFU,
            selective.rows.j$CFU,
            R = nonselective_cell_counts.j/nonselective_cell_counts.i
          )
        } else {
          this.result = rsalvador::LRT.LD.plating(
            selective.rows.i$CFU,
            selective.rows.j$CFU,
            R = nonselective_cell_counts.j/nonselective_cell_counts.i,
            e1 = selective_fraction.i,
            e2 = selective_fraction.j
          )
        }
        this.p.value = this.result[2]

        cat("  p-value:", this.p.value, "\n")

        comparison_data = rbind(comparison_data, data.frame(strain.1 = this.strain.i,
                                                            strain.2 = this.strain.j,
                                                            p.value = this.p.value))

      }
    }

    # Export comparisons CSV
    readr::write_csv(comparison_data, paste0(outputPath, "/", outputPrefix, ".comparisons.csv"))
  }


  return(invisible())

}
