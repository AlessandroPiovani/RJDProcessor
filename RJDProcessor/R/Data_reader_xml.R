# Definizione della classe S4
setClass("Data_reader_xml",
         slots = list(
           input_source     = "ANY",
           read_data        = "function",
           ...              = "ANY"
         ))


# read_data method, IT MUST RETURN AN mts OBJECT!!! MODIFY THIS METHOD TO CUSTOMIZE INPUT
#setGeneric("read_data", function(object, ...) standardGeneric("read_data"))
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

# Definizione del costruttore R-like
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

