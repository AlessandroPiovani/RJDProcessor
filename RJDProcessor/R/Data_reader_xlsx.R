# Definition of S4 class
#' @export
setClass("Data_reader_xlsx",
         slots = list(
           input_source     = "ANY",
           read_data        = "function",
           ...              = "ANY"
         ))


#' Get the data from a Data_reader_csv
#'
#' This function returns the data from the input_source of the object.
#'
#' @return data in form of numeric matrix, with rownames = dates (in string format, YYYY-MM-DD) and colnames = time series names (string)
#' @examples
#' input_data_file_name <- system.file("extdata","XLSX-TUR/grezzi_trim_TUR.xlsx", package = "RJDProcessor")
#' input_data_reader    <- Data_reader_xlsx(input_source = input_data_file_name)
#' input_data_reader@read_data()
#' @export
setMethod ("read_data", signature("Data_reader_xlsx"),
          function(object, ...) {

              require("readxl")


              suppressMessages({
                data <- read_excel(object@input_source, sheet = 1, col_names = TRUE)
                closeAllConnections()
              })

              # Removes "" frim column names and from values
              names(data) <- gsub("\"", "", names(data))
              #data[] <- lapply(data, function(x) gsub("\"", "", x))

              timestamps   <- as.Date(data[[1]])

              # Remove dates column
              data <- data[,-1]

              series_names <-names(data)



              # Create an mts object
              mts <- matrix(NA, ncol = length(series_names), nrow = length(timestamps))
              colnames(mts) <- series_names
              rownames(mts) <- as.character(timestamps)

              # auto detection of the time series frequency
              d1         <- as.Date(timestamps[1])
              d2         <- as.Date(timestamps[2])
              month_diff <- abs(as.numeric(format(d1, "%m")) - as.numeric(format(d2, "%m")))
              freq       <- 12/month_diff


              # Loop to create a ts obj for each column
              for (i in 1:ncol(mts)) {
                  suppressWarnings({
                    data[[i]] <- as.numeric(data[[i]])  # Convert string "NA" to numeric NA
                  })
                  mts[, i] <- ts(data[[i]], start = timestamps[1], frequency = freq)
              }

              #browser()


              return(mts)

          })

#' Constructor (R-like) of the Data_reader object
#'
#' This function creates a Data_reader object capable of reading data from XLSX files and returning it using the \code{read_data()} function.
#'
#' @param input_source A string with file name (also with path).
#' @return The Data_reader_xlsx object
#' @examples
#' input_data_file_name <- system.file("extdata","XLSX-TUR/grezzi_trim_TUR.xlsx", package = "RJDProcessor")
#' input_data_reader    <- Data_reader_xlsx(input_source = input_data_file_name)
#' input_data_reader@read_data()
#' @export
Data_reader_xlsx <- function(input_source = NA, ...) {

  obj <- new("Data_reader_xlsx", input_source = input_source, read_data = function() read_data(obj))

  return(obj)
}


