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

             suppressMessages({
               data <- read.csv(object@input_source, header = TRUE, sep = ";",
                                check.names = FALSE)
               closeAllConnections()
             })

             names(data) <- gsub("\"", "", names(data))

             # Parsing delle date: prova più formati
             raw_dates  <- trimws(as.character(data[[1]]))
             timestamps <- as.Date(raw_dates, format = "%d/%m/%Y")
             if (all(is.na(timestamps))) timestamps <- as.Date(raw_dates, format = "%Y-%m-%d")
             if (all(is.na(timestamps))) timestamps <- as.Date(raw_dates, format = "%Y/%m/%d")
             if (any(is.na(timestamps)))
               stop("Impossibile interpretare le date nella prima colonna (es. '",
                    raw_dates[which(is.na(timestamps))[1]], "')")

             data <- data[, -1, drop = FALSE]
             series_names <- names(data)

             mts <- matrix(NA_real_, ncol = length(series_names), nrow = length(timestamps))
             colnames(mts) <- series_names
             rownames(mts) <- as.character(timestamps)

             # Frequenza: differenza in mesi "assoluti" (anno*12 + mese)
             month_index <- as.integer(format(timestamps, "%Y")) * 12 +
               as.integer(format(timestamps, "%m"))
             month_diff  <- unique(diff(month_index))
             if (length(month_diff) != 1 || !(month_diff %in% c(1, 3, 6, 12)))
               stop("Date non regolari: passi in mesi trovati = ",
                    paste(month_diff, collapse = ", "))
             freq <- 12 / month_diff

             start_ts <- c(as.integer(format(timestamps[1], "%Y")),
                           (as.integer(format(timestamps[1], "%m")) - 1) %/% month_diff + 1)

             for (i in seq_len(ncol(mts))) {
               suppressWarnings({
                 x <- gsub(",", ".", data[[i]])
                 x <- as.numeric(x)
               })
               mts[, i] <- ts(x, start = start_ts, frequency = freq)
             }

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


