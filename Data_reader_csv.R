# Definizione della classe S4
setClass("Data_reader_csv", 
         slots = list(
           input_source     = "ANY",
           read_data        = "function",
           ...              = "ANY"
         ))


# read_data method, IT MUST RETURN AN mts OBJECT!!! MODIFY THIS METHOD TO CUSTOMIZE INPUT
setGeneric("read_data", function(object, ...) standardGeneric("read_data"))
setMethod ("read_data", signature("Data_reader_csv"),
          function(object, ...) {
              
              # Importa il file CSV
              #browser()
              suppressMessages({
                #data <- read_excel(object@input_source, sheet = 1, col_names = TRUE)
                data <- read.csv(object@input_source, header = TRUE, sep=";")
                closeAllConnections()
              })
              
              # Rimuovi le virgolette dai nomi delle colonne e dai valori
              names(data) <- gsub("\"", "", names(data))
              #data[] <- lapply(data, function(x) gsub("\"", "", x))
              
              timestamps   <- as.Date(data[[1]], format = "%d/%m/%Y")
              
              # Rimuovi la colonna delle date
              data <- data[, -1, drop = FALSE] # Remove the column with dates
              
              series_names <-names(data)
              
              
              
              # Crea un oggetto mts
              mts <- matrix(NA, ncol = length(series_names), nrow = length(timestamps))
              colnames(mts) <- series_names
              rownames(mts) <- as.character(timestamps)
              
              # auto detection of the time series frequency
              d1         <- as.Date(timestamps[1])
              d2         <- as.Date(timestamps[2])
              month_diff <- abs(as.numeric(format(d1, "%m")) - as.numeric(format(d2, "%m")))
              freq       <- 12/month_diff
              
              
              # Ciclo per creare un oggetto ts per ogni colonna
              for (i in 1:ncol(mts)) {
                  suppressWarnings({
                    data[[i]] <- as.numeric(data[[i]])  # Convert string "NA" to numeric NA
                  })
                  mts[, i] <- ts(data[[i]], start = timestamps[1], frequency = freq)
              }

              #browser()  
              
              
              return(mts)
            
          })

# Definizione del costruttore R-like
Data_reader_csv <- function(input_source = NA, ...) {

  # Crea un oggetto della classe Data_reader_csv
  obj <- new("Data_reader_csv", input_source = input_source, read_data = function() read_data(obj))
  
  return(obj)
}


