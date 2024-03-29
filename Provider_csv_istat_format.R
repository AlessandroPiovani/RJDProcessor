# Definizione della classe S4
setClass("Provider_csv_istat_format", 
         slots = list(
           input_source     = "ANY",
           read_data        = "function"
         ))


# read_data method, IT MUST RETURN AN mts OBJECT!!! MODIFY THIS METHOD TO CUSTOMIZE INPUT
setGeneric("read_data", function(object) standardGeneric("read_data"))
setMethod ("read_data", signature("Provider_csv_istat_format"),
          function(object) {
            
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
              
              
              # Ciclo per creare un oggetto ts per ogni colonna
              for (i in 1:ncol(mts)) {
                mts[, i] <- ts(data[, i], start = timestamps[1], frequency = 4)
              }
              
              return(mts)
            
          })

# Definizione del costruttore R-like
Provider_csv_istat_format <- function(input_source = NA) {

  # Crea un oggetto della classe Provider_csv_istat_format
  obj <- new("Provider_csv_istat_format", input_source = input_source, read_data = function() read_data(obj))
  
  return(obj)
}


