# THE read_ext_reg_data METHOD MUST RETURN AN MTS!!


# Definizione della classe S4
setClass("Provider_ext_reg_tsplus", 
         slots = list(
           input_source      = "ANY",   # in our case source is a directory path where the regressors are allocated
           read_ext_reg_data = "function",
           read_ext_reg_info = "function"
         ))

# read_data method, IT MUST RETURN AN mts OBJECT!!! MODIFY THIS METHOD TO CUSTOMIZE INPUT
setGeneric("read_ext_reg_data", function(object, var_info=NULL, time_series_info=NULL) standardGeneric("read_ext_reg_data"))
setMethod ("read_ext_reg_data", signature("Provider_ext_reg_tsplus", "ANY", "ANY"),
           function(object, var_info=NULL, time_series_info=NULL) {
             
             series_name <- time_series_info
             
             #var_info  = object@var_info
             #source    = object@source
             reg_directory   <- object@input_source
             #browser()

             if(is.null(var_info) || length(var_info)==0)
             {
               return(NA)
             }
             #browser()
             
             time_series_list <-list()
             # Itera attraverso gli elementi di var_info_list
             for (i in seq_along(var_info[[series_name]])) {
               
               #browser()
               user_def_var <- var_info[[series_name]][[i]]
               # Costruisci il percorso completo del file
               full_file_path <- file.path(reg_directory, user_def_var$file_name)
               
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
             
             if(is.null(mts_object))
             {
               mts_object = NA
             }   
             
             # Restituisci la mts
             return(mts_object)
             
           })

# read_ext_reg_info method, IT MUST RETURN A LIST of information on external regressors
setGeneric("read_ext_reg_info", function(object, var_info_container) standardGeneric("read_ext_reg_info"))
setMethod ("read_ext_reg_info", signature("Provider_ext_reg_tsplus", "ANY"),
           function(object, var_info_container) {
             
             workspace <- var_info_container 
                   
             jmodel <- RJDemetra::get_jmodel(workspace, progress_bar = TRUE) # to retrieve external regressors by name

             all_jmodel_vars <- getUserDefinedTdVariables_info(jmodel) # per editare la scrittura
             
             return(all_jmodel_vars)

           })


# Definizione del costruttore R-like
Provider_ext_reg_tsplus <- function(input_source) {
  
  obj <- new("Provider_ext_reg_tsplus", input_source = input_source, read_ext_reg_data = function(...) read_ext_reg_data(obj, ...), read_ext_reg_info = function(...) read_ext_reg_info(obj,...))
  
  # Restituisci l'oggetto creato
  return(obj)
}

















# The variables values are available in files.
# input_mode = TS_regressor_file the start date is the same as the input data series
# input_mode = TODO JD_regressor_file file containing both dates and data
getUserDefinedTdVariables_info <- function(jmodel ,input_mode=c("TS_regressor_file", "JD_regressor_file", ))
{
  var_info_list = list()
  #browser()
  for(name in names(jmodel[[1]]))
  {
    jSA_series      <- jmodel[[1]][[name]]
    jRegression     <- jSA_series$spec$getRegression()
    jCalendar       <- jRegression$getCalendar()
    jTradingDays    <- jCalendar$getTradingDays()
    jUserVarsString <- jTradingDays$getUserVariables()
    
    if(length(jUserVarsString)>0)
    {
      var_info_list[[name]] = list()
      
      file_list = list()
      i=1
      for(varString in jUserVarsString)
      {
        file_name <- tolower(sub(".*\\.(.*?)_\\d+$", "\\1", varString))
        file_name <- paste0(file_name, ".txt")
        # file_list[[i]] = list(file_name = nome_file, start=list(start(get_ts(jSA_series))) ,frequency=frequency(get_ts(jSA_series)))
        # browser()
        
        year  <- start(get_ts(jSA_series))[1]
        month <- start(get_ts(jSA_series))[2]
        start_date <- as.Date(paste(year, month, "01", sep = "-"))
        
        
        start_date <- format(start_date, "%Y-%m-%d")
        file_list[[i]] = list(file_name = file_name, start = start_date ,frequency=frequency(get_ts(jSA_series)))
        
        i=i+1
      }
      var_info_list[[name]] <- append(var_info_list[[name]], file_list)
    }  
  }  
  return(var_info_list)
}  


