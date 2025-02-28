% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{export_mut_plot}
\alias{export_mut_plot}
\title{Helper function saving mutation rate plots.}
\usage{
export_mut_plot(plot, prefix, outputPath)
}
\arguments{
\item{plot}{The ggplot object to save.}

\item{prefix}{The name of the file, which should be project name if handled correctly by the
calling function.}

\item{outputPath}{The path of the output folder. Also handled by the calling function.}
}
\description{
This function is called by \code{\link[=plot_fluxxer]{plot_fluxxer()}}. All it does is save a plot in PNG and PDF
format in "postcard" dimensions (6" x 8") to the specified output folder.
}
\keyword{internal}
