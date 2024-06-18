# Definizione della classe S4
setClass("Data_reader_csv_istat_format", 
         slots = list(
           input_source     = "ANY",
           read_data        = "function",
           ...              = "ANY"
         ))


# read_data method, IT MUST RETURN AN mts OBJECT!!! MODIFY THIS METHOD TO CUSTOMIZE INPUT
setGeneric("read_data", function(object, ...) standardGeneric("read_data"))
setMethod ("read_data", signature("Data_reader_csv_istat_format"),
          function(object, ...) {
            
              # Leggi i dati dal file specificato nel campo file_name_with_path 
              # Importa il file CSV
              data <- read.csv(object@input_source, header = FALSE, sep = ";", dec = ",", quote = "")
              
              # Rimuovi le virgolette dai nomi delle colonne e dai valori
              names(data) <- gsub("\"", "", names(data))
              data[] <- lapply(data, function(x) gsub("\"", "", x))
              
              # Converti le date nel formato "ANNOqMESE" in un formato R
              timestamps   <- as.Date(gsub("([0-9]+)q([0-9]+)", "\\1-\\2-01", data[[1]]))
              
              # Rimuovi il valore NA che corrisponde al nome della colonna delle date
              timestamps   <- timestamps[-1]
              # Rimuovi la colonna delle date
              data <- data[,-1]
              
              series_names <-unlist(as.list(data[1,]))
              # Rimuovi la riga dei nomi
              data <- data[-1,]
              
              
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
                mts[, i] <- ts(data[, i], start = timestamps[1], frequency = freq)
              }
              
              return(mts)
            
          })

# Definizione del costruttore R-like
Data_reader_csv_istat_format <- function(input_source = NA, ...) {

  # Crea un oggetto della classe Data_reader_csv_istat_format
  obj <- new("Data_reader_csv_istat_format", input_source = input_source, read_data = function() read_data(obj))
  
  return(obj)
}


