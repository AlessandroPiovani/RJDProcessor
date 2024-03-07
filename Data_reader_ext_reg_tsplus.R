# THE read_ext_reg_data METHOD MUST RETURN AN MTS!!


# Definizione della classe S4
setClass("Data_reader_ext_reg", 
         slots = list(
           var_info  = "ANY",
           source    = "ANY"   # in our case source is a directory path
         ))

# read_data method, IT MUST RETURN AN mts OBJECT!!! MODIFY THIS METHOD TO CUSTOMIZE INPUT
setGeneric("read_ext_reg_data", function(object) standardGeneric("read_ext_reg_data"))
setMethod ("read_ext_reg_data", signature("Data_reader_ext_reg"),
           function(object) {

               var_info  = object@var_info
               source    = object@source
             
               if(is.null(var_info) || length(var_info)==0)
               {
                 return(NA)
               }  
               # browser()
               
               time_series_list <-list()
               # Itera attraverso gli elementi di var_info_list
               for (i in seq_along(var_info)) {
                 
                 user_def_var <- var_info[[i]]
                 # Costruisci il percorso completo del file
                 full_file_path <- file.path(source, user_def_var$file_name)
                 
                 # Leggi i dati dal file
                 data <- scan(full_file_path, what = numeric(), sep = "\n")
                 
                 # Definisci la data di inizio e la frequenza
                 data_date <- as.Date(user_def_var$start)
                 y <- as.integer(format(data_date, "%Y"))
                 m <- as.integer(format(data_date, "%m"))
                 start_date <- c(y, m)
                 
                 frequency  <- user_def_var$frequency
                 
                 # Crea la serie temporale
                 time_series <- ts(data, start = start_date, frequency = frequency)
                 
                 # Aggiungi la serie temporale alla lista
                 time_series_list[[user_def_var$file_name]] <- time_series
               }
               
               # Converti la lista di serie temporali in una mts
               mts_object <- do.call(ts.union, time_series_list)
               
               # Restituisci la mts
               return(mts_object)
  
           })


# Definizione del costruttore R-like
Data_reader_ext_reg <- function(var_info, source) {
  
  # Crea un oggetto della classe Csv_istat_format_data_reader
  obj <- new("Data_reader_ext_reg", var_info=var_info, source=source)
  
  # Restituisci l'oggetto creato
  return(obj)
}





