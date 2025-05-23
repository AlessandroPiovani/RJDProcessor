% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/workspaces_manager.R
\name{update_data}
\alias{update_data}
\title{Update the data of a workspace}
\usage{
update_data(workspace_xml_path, data_reader)
}
\arguments{
\item{workspace_xml_path}{Path of the xml file of the workspace whose data
have to be updated}

\item{data_reader}{A Data_reader object already initialized with its input
source}
}
\description{
This function update the data of a workspace's time series basing on the data
read by a Data_reader object already initialized. The time series read by the
Data_reader must have the same colnames as the time series names of the workspace
to produce an update
}
\examples{
require(RJDemetra)
original_directory <- getwd()
extdata_directory  <- system.file("extdata", package = "RJDProcessor")
setwd(extdata_directory)
ws_xml_path  <- "TUR_ws_test_container/merged_ws.xml"
dr <- RJDProcessor::Data_reader_csv(input_source = "rawdata_TUR.csv")
# num_mat<-dr@read_data() # to check if the data are available
update_data(ws_xml_path, dr)
setwd(original_directory)
}
