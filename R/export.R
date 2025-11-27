#' Plot mutation rate data
#'
#' Produce a dot plot of estimated mutation rates, with error bars representing upper and lower
#' 95% confidence intervals.
#'
#' @details
#' This function can be supplied either a single CSV file produced by [`run_fluxxer()`] or a folder
#' of pooled/unpooled CSVs produced by that method. In the latter case, matching pooled and unpooled
#' data will be combined into a single plot showing the mutation rates pooled and by strain. Plots
#' are saved as 6 inch by 8 inch images, in both PNG and PDF format, to `/luria_output/analyzed/`.
#' Note that this function does not take dataframes.
#'
#' @inheritParams prep_export
#' @param dataPath A path to an output CSV file to plot mutation rates from. Alternatively, a folder
#' path containing pooled/unpooled file pairs.
#' @param export If `TRUE`, plots are exported as PDF and PNG files.
#'
#' @examples
#' plot <- plot_fluxxer(dataPath = "./data/analyzed/wrangled.output.csv",
#'                      overwrite = TRUE)
#'
#' @return
#' A single `ggplot2` object, or a list of `ggplot2` objects, each one corresponding to a pair of
#' pooled and unpooled input data.
#'
#' @export

plot_fluxxer <- function(dataPath, export = TRUE, overwrite = TRUE, export.to = "."){

  if(!is.character(dataPath)){
    stop("This function wants a CSV file or a folder of such.")
  }

  # Prep export folder and handle overwriting
  outputPath <- prep_export(mode = "plots", outputParent = export.to, overwrite)

  # Detect input type (single or folder)
  if(is.character(dataPath)){
    if(utils::file_test("-d", dataPath)){ # dataPath is dir
      isDirectory <- TRUE
    } else {
      isDirectory <- FALSE
    }
  } else {
    stop("This function wants a file or folder path as input.")
  }

  # Begin wrangle
  if(isFALSE(isDirectory)){ # single file supplied

    # wrangle & plot
    plotData <- wrangle_plot_data(file = dataPath)
    plot <- plot_mutrates(data = plotData$data, levelOrder = plotData$levels, log = plotData$log)

    # grep "project name" file prefix and export
    prefix <- sub(".output.csv$", "", basename(dataPath))
    if(export){
      export_mut_plot(plot, prefix, outputPath)
      message("Plot saved to /analyzed/.\n")
    }

    # for return
    plots <- plot

  } else if(isDirectory){ # folder with filepairs

    # grep "project name" file prefixes
    # plotting is by project, so pooled files imply unpooled filenames too
    pooledList <- grep("_pooled.output.csv", dir(dataPath), value = TRUE)
    projectList <- sub("_pooled.output.csv$", "", pooledList)

    if(length(projectList) == 0){
      stop("No pooled estimates found. If supplying a folder, it must have pooled/unpooled files.")
    }

    # loop and plot over all projects
    plots <- list()
    for(projectName in projectList){
      plotData <- wrangle_plot_data(file = NULL, projectName, dataPath)
      plot <- plot_mutrates(data = plotData$data, levelOrder = plotData$levels, log = plotData$log)

      # export
      if(export){
        export_mut_plot(plot, prefix = projectName, outputPath)
      }

      # construct return object
      plots[[projectName]] <- plot

    }

    if(export){
      message("Plots saved to /analyzed/.\n")
    }

  }


  return(plots)

}


#' The core plotting function
#'
#' Produce a ggplot object with or without a log-scale y-axis.
#'
#' @details
#' This function does nothing but produce the *luria* plot. Factoring the replicates is done by
#' [`refactor_reps()`], and switching the log y-axis is done through a logical value supplied by
#' [`test_logticks()`]. [`plot_fluxxer()`] calls this function every time a plot is generated.
#'
#' @import ggplot2 forcats
#'
#' @param data A wrangled data object to use for plotting.
#' @param levelOrder A string vector defining the order that replicates will appear on the x-axis.
#' @param log A logical value indicating if the data will trigger the [`annotation_logticks`] error.
#' Defaults to `NULL`; logticks are applied if `TRUE`.
#'
#' @return
#' A ggplot2 object.
#'
#' @keywords internal

plot_mutrates <- function(data, levelOrder, log = NULL){

  # make most of the plot
  plot <- ggplot2::ggplot(data, aes(x = forcats::fct_relevel(strain, levelOrder),
                                    y = mu)) +
    geom_point(shape = 16, size = 2, position = position_dodge(width = 0.75)) +
    geom_errorbar(aes(ymin=CI.95.lower, ymax=CI.95.higher), width = 0.10, linewidth = 0.4,
                  position = position_dodge(width = 0.75)) +
    scale_y_log10() +
    xlab(NULL) +
    ylab("Mutation rate") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

  # if this flag is true, logticks can be added and won't cause an error
  if(isTRUE(log)){
    plot <- plot + annotation_logticks(sides = "l")
  }


  return(plot)

}


#' Extract mutation rates in a reproducible manner
#'
#' Automatically get mutation rates from output data and save it.
#'
#' @details
#' Extracts pooled mutation rate estimates from multiple projects of pooled/unpooled file pairs
#' into a vector named by project, and exports them as a CSV of project name and estimates, and/or
#' an RData file. Likely only useful for extracting mutation rates from multiple projects at once.
#' This function does not take single files, and specifically looks for pooled estimate data as it
#' really only makes sense to use those.
#'
#' @inheritParams prep_export
#' @param dataPath Path to a folder containing pooled mutation rates to extract.
#' @param export.method Takes value `"csv"`, which exports the data as a CSV, `"data"`, which
#' exports the data as an RData object; or `"all"`, which exports all formats.
#'
#' @examples
#' mutrates <- extract_mutrates(export.method = "all")
#'
#' @return
#' A numeric vector with elements named by project.
#'
#' @export

extract_mutrates <- function(dataPath, export.method = "all", overwrite = FALSE, export.to = "."){

  # Prep output
  outputPath <- prep_export(mode = "mutrates", outputParent = export.to, overwrite)

  # make sure input is a directory
  if(!utils::file_test("-d", dataPath)){
    stop("This function wants a folder path, not a file.")
  }

  # Grep "project name" file prefixes
  pooledList <- grep("_pooled.output.csv", dir(dataPath), value = TRUE)
  projectList <- sub("_pooled.output.csv$", "", pooledList)
  if(length(projectList) == 0){
    stop("Could not find any pooled mutation rates to extract.")
  }

  # Extract mutation rates
  mutrateCSV <- data.frame(project = rep(NA, length(projectList)), mutation_rate = NA)
  mutrateDat <- c()
  for(i in 1:length(projectList)){
    projectName <- projectList[i]
    pooledData <- read.csv(paste0(dataPath, "/", projectName, "_pooled.output.csv"), header = T)
    mutrateCSV$project[i] <- projectName
    mutrateCSV$mutation_rate[i] <- pooledData$mu
    mutrateDat[[projectName]] <- pooledData$mu
  }

  # Begin export
  timestamp <- format(Sys.Date(), "%Y-%m-%d")
  if(export.method == "csv" | export.method == "all"){
    write.csv(mutrateCSV, file = paste0(outputPath, "/", "mutrate_", timestamp, ".csv"))
    message("Mutation rate CSV saved to /analyzed/.")
  }
  if(export.method == "data" | export.method == "all"){
    save(mutrateDat, file = paste0(outputPath, "/", "mutrate_", timestamp, ".rda"))
    message("Mutation rate RData file saved to /analyzed/.")
  }


  return(mutrateDat)

}


#' Save a copy of the fluctuation analysis data template
#'
#' Use this function to save a copy of the fluctuation analysis Excel data template.
#'
#' @param version Which version of the template to use. Defaults to `latest`; `old` saves a copy of
#' the old template - use this at your own risk.
#' @param save.as What the template file will be named. The default is "template".
#' @param export.to Where to save the template file. Defaults to current directory.
#'
#' @examples
#' get_template("./data", "my_data_template")
#'
#' @export

get_template <- function(version = "latest", save.as = "template", export.to = "."){

  # NTS:
  # Low-level file manipulation across both Windows and UNIX is hell, so just reading in
  #   the file and saving it back out.
  # This function doesn't really use the established helper tools in the rest of the pipeline.
  # Exporting arguably should use new option in prep_export. Maybe later.

  # Read the file in
  if(version == "latest"){
    wb <- openxlsx::loadWorkbook(system.file("extdata", "fluctest_template.xlsx",
                                             package = "luria"))
  } else if(version == "old"){
    wb <- openxlsx::loadWorkbook(system.file("extdata", "fluctest_template_old.xlsx",
                                             package = "luria"))
  }

  # Export
  # Create export dir; recursive creates missing dirs leading to export dir
  dir.create(export.to, recursive = TRUE, showWarnings = FALSE)

  # Handle savename
  if(!is.null(save.as)){ # save.as supercedes all other names
    exportName <- paste0(export.to, "/", save.as)
  } else {
    stop("save.as is NULL, how did this happen?")
  }

  # Save the file back out
  exportPath <- paste0(exportName, ".xlsx")
  openxlsx::saveWorkbook(wb, exportPath)
  message(paste0("Data template saved to ", here::here(exportPath)))


  return(invisible())

}
