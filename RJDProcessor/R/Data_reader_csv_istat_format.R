# Definition of S4 class
setClass("Data_reader_csv_istat_format",
         slots = list(
           input_source     = "ANY",
           read_data        = "function",
           ...              = "ANY"
         ))


#' Get the data from a Data_reader_csv_istat_format
#'
#' This function returns the data from the input_source of the object.
#'
#' @return data in form of numeric matrix, with rownames = dates (in string format, YYYYqMM) and colnames = time series names (string)
#' @examples
#' input_data_file_name <- system.file("extdata","SITIC-TUR/grezziTUR.csv", package = "RJDProcessor")
#' input_data_reader    <- Data_reader_csv_istat_format(input_source = input_data_file_name)
#' input_data_reader@read_data()
#' @export
setMethod ("read_data", signature("Data_reader_csv_istat_format"),
          function(object, ...) {

              # Read the data from the file whose name is specified by file_name_with_path
              # Import CSV file
              data <- read.csv(object@input_source, header = FALSE, sep = ";", dec = ",", quote = "")

              # Removes "" from colnames and from values
              names(data) <- gsub("\"", "", names(data))
              data[] <- lapply(data, function(x) gsub("\"", "", x))

              # Convert dates in format "YEARqMONTH" to an R format
              timestamps   <- as.Date(gsub("([0-9]+)q([0-9]+)", "\\1-\\2-01", data[[1]]))

              # Removes NA value that is the name of the column of the dates
              timestamps   <- timestamps[-1]
              # Remove dates column
              data <- data[,-1]

              series_names <-unlist(as.list(data[1,]))
              # Removes names row
              data <- data[-1,]


              # Creates an mts object
              mts <- matrix(NA, ncol = length(series_names), nrow = length(timestamps))
              #mts <- gsub(",", ".", mts)

              colnames(mts) <- series_names
              rownames(mts) <- as.character(timestamps)
              r_names <-  rownames(mts)
              c_names <-  colnames(mts)

              # auto detection of the time series frequency
              d1         <- as.Date(timestamps[1])
              d2         <- as.Date(timestamps[2])
              month_diff <- abs(as.numeric(format(d1, "%m")) - as.numeric(format(d2, "%m")))
              freq       <- 12/month_diff


              # Loop to create a ts object for each column
              for (i in 1:ncol(mts)) {
                mts[, i] <- ts(data[, i], start = timestamps[1], frequency = freq)
              }

              #browser()
              mts <- gsub(",", ".", mts)
              numeric_matrix <- matrix(unlist(lapply(mts, as.numeric)), nrow = nrow(mts), ncol = ncol(mts))
              rownames(numeric_matrix) <- r_names
              colnames(numeric_matrix) <- c_names

              #return(mts)
              return(numeric_matrix)
          })

#' Constructor (R-like) of the Data_reader object
#'
#' This function creates a Data_reader object capable of reading data from CSV files in ISTAT format and returning it using the \code{read_data()} function.
#' The ISTAT format is a csv file with dates in format YYYYqMM as rownames and time_series names as colnames
#'
#' @param input_source A string with file name (also with path).
#' @return The Data_reader_csv_istat_format object
#' @examples
#' input_data_file_name <- system.file("extdata","SITIC-TUR/grezziTUR.csv", package = "RJDProcessor")
#' input_data_reader <- Data_reader_csv_istat_format(input_source = input_data_file_name)
#' #input_data_reader@read_data()
#' @export
Data_reader_csv_istat_format <- function(input_source = NA, ...) {

  obj <- new("Data_reader_csv_istat_format", input_source = input_source, read_data = function() read_data(obj))

  return(obj)
}


