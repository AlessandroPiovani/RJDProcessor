% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report.R
\name{create_diagnostic_report2}
\alias{create_diagnostic_report2}
\title{Create another Diagnostic Report for Time Series Models}
\usage{
create_diagnostic_report2(workspace, output_file = "series_info.txt")
}
\arguments{
\item{workspace}{A workspace object containing the time series models.}

\item{output_file}{A string specifying the output file path where the report will be saved (default is "report.txt").}
}
\description{
This function generates a diagnostic report for time series models.
It shows for every time series a complete view, including regression coefficients
with their T-statistics as well as LB, LB2, Normality tests and BIC
}
\examples{
require(RJDemetra)

original_directory <- getwd()
extdata_directory  <- system.file("extdata", package = "RJDProcessor")
setwd(extdata_directory)
ws_path <- "WorkspaceTUR-container/workspace-TUR.xml"

workspace <- load_workspace(file=ws_path)
create_diagnostic_report2(workspace, output_file = "report.out")
setwd(original_directory)
}
