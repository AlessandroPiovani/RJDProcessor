# Definition of S4 class
setClass("Data_reader_xml",
         slots = list(
           input_source     = "ANY",
           read_data        = "function",
           ...              = "ANY"
         ))

#' Get the data from a Data_reader_xml
#'
#' This function returns the data from the input_source of the object.
#'
#' @return data in form of numeric matrix, with rownames = dates (in string format, YYYY-MM-DD) and colnames = time series names (string)
#' @examples
#' input_data_file_name <- system.file("extdata","Prod.xml", package = "RJDProcessor")
#' # NOTE: absolute paths are better for this Data_reader
#' input_data_reader    <- Data_reader_xml(input_source = input_data_file_name)
#' #input_data_reader@read_data() # for reading the data
#' @export
setMethod ("read_data", signature("Data_reader_xml"),
          function(object, ...) {

              #input_source<-"C:\\Users\\UTENTE\\Desktop\\RJDProcessor_package\\test\\Prod.xml"

              xml_cont<-rjd3providers::xml_data(object@input_source)

              series <- xml_cont$series
              time_ser_names<-list()
              time_ser_data <-list()

              for(time_ser in series)
              {
                time_ser_names <- append(time_ser_names, time_ser$name)
                ts <- time_ser$data
                time_ser_data  <- append(time_ser_data,  list(ts))
              }

              mts_data <- do.call(cbind, time_ser_data)

              colnames(mts_data) <- time_ser_names

              start_date <- start(mts_data)

              end_date <- end(mts_data)

              start_year <- start_date[1]
              start_month <- start_date[2]
              end_year <- end_date[1]
              end_month <- end_date[2]

              # Creates a data vector using seq
              dates_seq <- seq(from = as.Date(paste(start_year, start_month, 1, sep = "-")),
                               to = as.Date(paste(end_year, end_month, 1, sep = "-")),
                               by = "month")

              # Dates in string, formatted as "YYYY-mm-dd"
              formatted_dates <- format(dates_seq, "%Y-%m-%d")

              rownames(mts_data) <- formatted_dates

              return(mts_data)

          })


#' Constructor (R-like) of the Data_reader object
#'
#' This function creates a Data_reader object capable of reading data from XLSX files and returning it using the \code{read_data()} function.
#'
#' @param input_source A string with file name (also with path).
#' @return The Data_reader_xlsx object
#' @examples
#' input_data_file_name <- system.file("extdata","Prod.xml", package = "RJDProcessor")
#' # NOTE: absolute paths are better for this Data_reader
#' input_data_reader    <- Data_reader_xml(input_source = input_data_file_name)
#' #input_data_reader@read_data() # for reading the data
#' @export
Data_reader_xml <- function(input_source = NA, ...) {

  rjd3providers_present <- require(rjd3providers)
  if(!rjd3providers_present)
  {
    print("rjd3providers package is needed to use the xml data_reader, make sure to have it installed on your pc")
    return(NULL)
  }

  obj <- new("Data_reader_xml", input_source = input_source, read_data = function() read_data(obj))

  return(obj)
}


# TEST
# xml_reader <- Data_reader_xml(input_source = "C:\\Users\\UTENTE\\Desktop\\RJDProcessor_package\\test\\Prod.xml")
# data<-xml_reader@read_data()

