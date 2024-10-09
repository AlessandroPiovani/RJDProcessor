# Definition of S4 class
setClass("Data_reader_csv",
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
#' input_data_file_name <- system.file("extdata","CSV-FAS/grezzi_trim_FAS.csv", package = "RJDProcessor")
#' input_data_reader    <- Data_reader_csv(input_source = input_data_file_name)
#' input_data_reader@read_data()
#' @export
setMethod ("read_data", signature("Data_reader_csv"),
          function(object, ...) {

              # Import CSV file
              #browser()
              suppressMessages({
                #data <- read_excel(object@input_source, sheet = 1, col_names = TRUE)
                data <- read.csv(object@input_source, header = TRUE, sep=";")
                closeAllConnections()
              })

              # Removes "" from column names and values
              names(data) <- gsub("\"", "", names(data))
              #data[] <- lapply(data, function(x) gsub("\"", "", x))

              timestamps   <- as.Date(data[[1]], format = "%d/%m/%Y")

              # Remove the column of the dates
              data <- data[, -1, drop = FALSE] # Remove the column with dates

              series_names <-names(data)



              # Creates an mts object
              mts <- matrix(NA, ncol = length(series_names), nrow = length(timestamps))
              colnames(mts) <- series_names
              rownames(mts) <- as.character(timestamps)

              # auto detection of the time series frequency
              d1         <- as.Date(timestamps[1])
              d2         <- as.Date(timestamps[2])
              month_diff <- abs(as.numeric(format(d1, "%m")) - as.numeric(format(d2, "%m")))
              freq       <- 12/month_diff


              # Loop to create a ts object for each column
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
#' This function creates a Data_reader object capable of reading data from CSV files and returning it using the \code{read_data()} function.
#'
#' @param input_source A string with file name (also with path).
#' @return The Data_reader_csv object
#' @examples
#' input_data_file_name <- system.file("extdata","CSV-FAS/grezzi_trim_FAS.csv", package = "RJDProcessor")
#' input_data_reader    <- Data_reader_csv(input_source = input_data_file_name)
#' input_data_reader@read_data()
#' @export
Data_reader_csv <- function(input_source = NA, ...) {

  obj <- new("Data_reader_csv", input_source = input_source, read_data = function() read_data(obj))

  return(obj)
}


