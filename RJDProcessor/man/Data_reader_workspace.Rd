% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Data_reader_workspace.R
\name{Data_reader_workspace}
\alias{Data_reader_workspace}
\title{Constructor (R-like) of the Data_reader object}
\usage{
Data_reader_workspace(input_source = NA, ...)
}
\arguments{
\item{input_source}{A string with workspace xml file name (also with path).}
}
\value{
The Data_reader_workspace object
}
\description{
This function creates a Data_reader object capable of reading data from
workspaces and returning them using the \code{read_data()} function.
}
\examples{
input_data_file_name <- system.file("extdata","WorkspaceTUR-container/workspace-TUR.xml", package = "RJDProcessor")
input_data_reader    <- Data_reader_workspace(input_source = input_data_file_name)
mts <- input_data_reader@read_data()
}
