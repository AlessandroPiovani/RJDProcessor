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
               # data <- scan(full_file_path, what = numeric(), sep = "\n")
               data <- read.table(full_file_path, header = FALSE, sep = "\t")
               
               # Definisci la data di inizio e la frequenza
               data_date <- as.Date(user_def_var$start)
               y <- as.integer(format(data_date, "%Y"))
               m <- as.integer(format(data_date, "%m"))
               start_date <- c(y, m)
               
               frequency  <- user_def_var$frequency
               
               
               # if(series_name=="VATPIA")
               # {   
               #   browser()
               # }
               
               # Crea la serie temporale
               time_series <- stats::ts(data, start = start_date, frequency = frequency)
               
               #if(ncol(time_series)>1) # last comment
               #{ 
                 #colnames_to_append <- paste(colnames(time_series), user_def_var$file_name, sep = "_")
                 # append file_name only if it is not already present
                 #colnames(time_series) <- ifelse(grepl(user_def_var$file_name, colnames(time_series)), colnames(time_series), colnames_to_append)
                 #browser()
                 #print("a")
                 colnames(time_series) <- paste(colnames(time_series), user_def_var$file_name, sep = "_")
               #}
               # 
               # Aggiungi la serie temporale alla lista
               time_series_list[[user_def_var$file_name]] <- time_series
             }
             #browser()
             # Converti la lista di serie temporali in una mts

             #if(length(time_series_list)!=1) #mts object obtained by ts_union loses its name if it is made by only one element, so I restore it
             #{
                mts_object <- do.call(ts.union, time_series_list)
             #}
             #else #mts object obtained by ts_union loses its name if it is made by only one element, so I restore it
             #{
               #browser()
             # mts_object <- matrix(as.vector(time_series_list[[1]]), ncol =  1)
             # mts_object <- ts(mts_object, start = start_date ,frequency = frequency)
             #  colnames(mts_object) <- names(c(time_series_list[1]))
               
             #} 
             if(is.null(mts_object))
             {
               mts_object = NA
             }
  
             # if(series_name=="VATAIC")
             # {
             #    browser()
             # }
             
             # Fix mts_column names (if in the case of 6 variables filename is repetaed before and after the variable number)     
             # browser()
             if (all(!is.na(mts_object)) && ncol(mts_object) > 1) 
             {
                colnames(mts_object) <- sub(".*\\.(V[1-6]_.*$)", "\\1", colnames(mts_object))             
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
  require(rJava)
  for(name in names(jmodel[[1]]))
  {
    jSA_series      <- jmodel[[1]][[name]]
    jRegression     <- jSA_series$spec$getRegression()
    jCalendar       <- jRegression$getCalendar()
    jTradingDays    <- jCalendar$getTradingDays()
    jUser_Td_VarsString      <- jTradingDays$getUserVariables()
    file_list <- list()
    
    idx_file_list = 1
    
      
    usrDefVarCount    <- jRegression$getUserDefinedVariablesCount()
   
    if(usrDefVarCount > 0)
    {
      var_info_list[[name]] = list()
      
      file_list = list()
      previous=list() 
      for(i in 1:usrDefVarCount)
      {
        
        jUser_UserDef_Var <- jRegression$getUserDefinedVariable(integer(i))
        varString         <- jUser_UserDef_Var$getName() 
        
        file_name  <- tolower(sub(".*\\.(.*?)_\\d+$", "\\1", varString))
        file_name  <- paste0(file_name, ".txt")
        
        year       <- start(get_ts(jSA_series))[1]
        month      <- start(get_ts(jSA_series))[2]
        start_date <- as.Date(paste(year, month, "01", sep = "-"))
        
        start_date <- format(start_date, "%Y-%m-%d")
        current    <- list(file_name = file_name, start = start_date, frequency=frequency(get_ts(jSA_series)))#, type="userdef")
        if (!identical(current, previous)) # for the 6 variables case: all the variables belong to the same file, so the fileinfo will be replicated 6 time, but i save only one of them not incrementing idx_file_list
        {
          file_list[[idx_file_list]] <- current
          previous = file_list[[idx_file_list]]
          idx_file_list=idx_file_list+1
        }
        
        
        
      }
      #var_info_list[[name]] <- append(var_info_list[[name]], file_list)
        
    }  
      
    
    if(length(jUser_Td_VarsString)>0)
    {
      # if (!(name %in% names(var_info_list)))
      # {
      #   var_info_list[[name]] = list()
      # }

      #i = length(var_info_list[[name]])+1 # if some usrDefVar have been already added, I start adding the TD user defined vars after them
      i = idx_file_list
      previous=list()
      for(varString in jUser_Td_VarsString)
      {
        file_name <- tolower(sub(".*\\.(.*?)_\\d+$", "\\1", varString))
        file_name <- paste0(file_name, ".txt")
        
        
        year  <- start(get_ts(jSA_series))[1]
        month <- start(get_ts(jSA_series))[2]
        start_date <- as.Date(paste(year, month, "01", sep = "-"))
        
        
        start_date <- format(start_date, "%Y-%m-%d")
        current = list(file_name = file_name, start = start_date, frequency=frequency(get_ts(jSA_series)))#, type="td")
        if (!identical(current, previous))  # for the 6 variables case: all the variables belong to the same file, so the fileinfo will be replicated 6 time, but i save only one of them not incrementing i
        {
          file_list[[i]] = current
          previous       = file_list[[i]]
          i=i+1
        }
         
      }
    }
    var_info_list[[name]] <- append(var_info_list[[name]], file_list)
    
  }  

  return(var_info_list)
}  


