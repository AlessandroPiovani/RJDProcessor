# Definizione della classe S4
setClass("Data_reader_list", 
         slots = list(
           input_source     = "ANY",
           read_data        = "function",
           ...              = "ANY"
         ))

setGeneric("read_data", function(object, ...) standardGeneric("read_data"))
setMethod ("read_data", signature("Data_reader_list"),
           function(object, ...) {
             
             # browser()
             
             input_data_list <- object@input_source
             
             
             # Function to identify the date format
             identify_date_format <- function(date_string) {
               if (grepl("/", date_string)) {
                 return("%d/%m/%Y")
               } else if (grepl("-", date_string)) {
                 return("%Y-%m-%d")
               } else {
                 stop("Unknown date format for string: ", date_string)
               }
             }
             
             # Convert dates to Date objects with appropriate format
             all_dates_chr <- c()
             all_dates     <- c()
             for (i in seq_along(input_data_list)) {
               all_dates_chr <- c(all_dates_chr, input_data_list[[i]]$dates)
               date_format   <- identify_date_format(input_data_list[[i]]$dates[1])  # Check format of the first date
               input_data_list[[i]]$dates <- as.Date(input_data_list[[i]]$dates, format = date_format)
               all_dates     <- c(all_dates, input_data_list[[i]]$dates)
             }
             
             # Find the earliest and latest dates
             start_date <- all_dates_chr[which.min(all_dates)]
             end_date   <- all_dates_chr[which.max(all_dates)]
             
             # Create a complete sequence of dates
             complete_dates <- seq.Date(from = as.Date(start_date, format= date_format), to = as.Date(end_date, format= date_format), by = "month")
             complete_dates <- format(complete_dates, "%Y-%m-%d")
             
             # Create an empty matrix with NA
             series_names <- sapply(input_data_list, function(x) x$series_name)
             mts_matrix <- matrix(NA, nrow = length(complete_dates), ncol = length(series_names))
             rownames(mts_matrix) <- complete_dates
             colnames(mts_matrix) <- series_names
             
             # Populate the matrix with values
             for (series in input_data_list) {
               series_dates <- format(series$dates, "%Y-%m-%d")
               series_values <- series$values
               series_name <- series$series_name
               mts_matrix[as.character(series_dates), series_name] <- series_values
             }
             
             return(mts_matrix)

           })

# # read_data method, IT MUST RETURN AN mts OBJECT!!! MODIFY THIS METHOD TO CUSTOMIZE INPUT
# setGeneric("read_data", function(object, ...) standardGeneric("read_data"))
# setMethod ("read_data", signature("Data_reader_list"),
#           function(object, ...) {
#               
#               browser()
#               
#               # Importa il file CSV
#               #browser()
#               data <- read.csv(object@input_source, header = TRUE, sep=";")
#               
#               # Rimuovi le virgolette dai nomi delle colonne e dai valori
#               names(data) <- gsub("\"", "", names(data))
#               #data[] <- lapply(data, function(x) gsub("\"", "", x))
#               
#               timestamps   <- as.Date(data[[1]], format = "%d/%m/%Y")
#               
#               # Rimuovi la colonna delle date
#               data <- data[, -1, drop = FALSE] # Remove the column with dates
#               
#               series_names <-names(data)
#               
#               
#               
#               # Crea un oggetto mts
#               mts <- matrix(NA, ncol = length(series_names), nrow = length(timestamps))
#               colnames(mts) <- series_names
#               rownames(mts) <- as.character(timestamps)
#               
#               # auto detection of the time series frequency
#               d1         <- as.Date(timestamps[1])
#               d2         <- as.Date(timestamps[2])
#               month_diff <- abs(as.numeric(format(d1, "%m")) - as.numeric(format(d2, "%m")))
#               freq       <- 12/month_diff
#               
#               
#               # Ciclo per creare un oggetto ts per ogni colonna
#               for (i in 1:ncol(mts)) {
#                   suppressWarnings({
#                     data[[i]] <- as.numeric(data[[i]])  # Convert string "NA" to numeric NA
#                   })
#                   mts[, i] <- ts(data[[i]], start = timestamps[1], frequency = freq)
#               }
# 
#               #browser()  
#               
#               
#               return(mts)
#             
#           })

# Definizione del costruttore R-like
Data_reader_list <- function(input_source = NA, ...) {

  # Crea un oggetto della classe Data_reader_csv
  obj <- new("Data_reader_list", input_source = input_source, read_data = function() read_data(obj))
  
  return(obj)
}


