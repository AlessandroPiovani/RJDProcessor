% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/workspaces_manager.R
\name{check_external_regressors}
\alias{check_external_regressors}
\title{Verify the external regressors used in a workspace}
\usage{
check_external_regressors(ws_single_ts)
}
\arguments{
\item{ws_single_ts}{The xml path of the workspace containing the data to be
checked}
}
\value{
Boolean: TRUE if ckeck is ok, FALSE if there are problems in the
external regressors.
}
\description{
This function check whether the external regressors used in a workspace are
up to date (i.e. they cover from the beginning to the end of the time series
and have the same frequency). The workspace must contain a single time series.
}
\examples{
require(RJDemetra)
original_directory <- getwd()
extdata_directory  <- system.file("extdata", package = "RJDProcessor")
setwd(extdata_directory)
single_workspaces_path <- "splitted_workspaces_to_merge"
single_ws<-load_workspace(paste0(single_workspaces_path,"\\\\VATPIC\\\\VATPIC.xml"))
compute(single_ws)
result<- check_external_regressors(single_ws)
setwd(original_directory)
}
