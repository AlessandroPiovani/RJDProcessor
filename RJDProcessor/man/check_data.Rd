% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/workspaces_manager.R
\name{check_data}
\alias{check_data}
\title{Compare the data in a workspace with only one time series with given data}
\usage{
check_data(raw_data, ws_single_ts, raw_data_start = NA, raw_data_freq = NA)
}
\arguments{
\item{raw_data}{Raw data to be compared with the ones in the workspace}

\item{ws_single_ts}{The xml path of the workspace containing the data to be
compared}

\item{raw_data_start}{-optional- Default=NA, the starting date of the raw data
in form "YYYY-MM-DD". If NA, the starting date is assumed
to be the same as the workspace data}

\item{raw_data_freq}{-optional- Default=NA, the frequency of the raw data (
e.g. 12=monthly, 4=quarterly). If NA, the frequency is
assumed to be the same as the workspace data}
}
\value{
Boolean: TRUE if data are the same, FALSE otherwise
}
\description{
This function compares the data in a workspace with only one time series with
given raw data in array form
}
\examples{
require(RJDemetra)
original_directory <- getwd()
extdata_directory  <- system.file("extdata", package = "RJDProcessor")
setwd(extdata_directory)
single_workspaces_path <- "splitted_workspaces_to_merge"
single_ws<-load_workspace(paste0(single_workspaces_path,"\\\\VATPIC\\\\VATPIC.xml"))
result<-check_data(c(1,2,3), single_ws)
setwd(original_directory)
}
