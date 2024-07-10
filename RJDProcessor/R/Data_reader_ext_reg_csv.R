# THE read_ext_reg_data METHOD MUST RETURN AN MTS!!
# Definizione della classe S4
#' @export
setClass("Data_reader_ext_reg_csv",
         slots = list(
           input_source      = "ANY",   # in our case source is a directory path where the regressors are allocated
           read_ext_reg_data = "function",
           read_ext_reg_info = "function",
           ...               = "ANY"
         ))

# read_data method, IT MUST RETURN AN mts OBJECT!!! MODIFY THIS METHOD TO CUSTOMIZE INPUT
#setGeneric("read_ext_reg_data", function(object, var_info=NULL, time_series_info=NULL, frequency= NA_integer_, ...) standardGeneric("read_ext_reg_data"))
#' @export
setMethod ("read_ext_reg_data", signature("Data_reader_ext_reg_csv", "ANY", "ANY", "ANY"),
           function(object, var_info=NULL, time_series_info=NULL, frequency=NA_integer_, ...) {

             #browser()

             series_name <- time_series_info

             reg_directory   <- object@input_source

             if(is.null(var_info) || length(var_info)==0)
             {
               return(NA)
             }

             ts_list <-list()


             mts_total <- NA
             # Itera attraverso gli elementi di var_info_list
             for (i in seq_along(var_info[[series_name]]))
             {

               #browser()
               user_def_var <- var_info[[series_name]][[i]]


               # Given a filename, check if there is a file in the path with the same name but with some uppercase letters,
               # which unfortunately are automatically converted to lowercase.
               # If the file being searched for is not present but a file that differs only by some uppercase letters is found, read that one.
               f_name <- find_file_case_insensitive(reg_directory, user_def_var$container)

               # Costruisci il percorso completo del file
               full_file_path <- file.path(reg_directory, f_name)


               # if(user_def_var$n_var != 1)
               # {
               col_types <- c("date", rep("numeric", user_def_var$n_var))
               # }
               # else {
               #   col_types <- c("date", "numeric")
               # }



               # Leggi i dati dal file
               suppressMessages({
                 data <- read.csv(full_file_path, header = TRUE, sep = ";")
                 #browser()
                 closeAllConnections()
               })

               dates_of_variables <- data[, 1]
               data <- data[, -1, drop = FALSE] # Remove the column with dates



               # Definisci la data di inizio e la frequenza
               start_date_ext_reg <- as.Date(user_def_var$start)
               y <- as.integer(format(start_date_ext_reg, "%Y"))
               m <- as.integer(format(start_date_ext_reg, "%m"))
               start_date <- c(y, m)


               # frequency  <- user_def_var$frequency # if I passed the frequency as a metadata
               if(is.na(frequency)) # Auto detect of frequency, not possible for TS txt format (this if is never used)
               {
                 #Auto detect of frequency from the data
                 #browser()
                 timestamps  <- rownames(data)
                 start_index <- which(!is.na(series))[1]
                 d1          <- as.Date(timestamps[start_index])
                 d2          <- as.Date(timestamps[start_index+1])
                 month_diff  <- abs(as.numeric(format(d1, "%m")) - as.numeric(format(d2, "%m")))
                 frequency   <- 12/month_diff
               }

               mts_file<-NA
               # ITERA SULLE COLONNE DEL FILE
               for (j in 1:ncol(data)) #one column is the date
               {
                 # browser()
                 # Ottieni il nome della colonna
                 column_name <- user_def_var$container
                 column_name <- gsub("\\.[cC][sS][vV]$", "", column_name)

                 # Ottieni i dati della colonna
                 column_data <- data[[j]]



                 #browser()

                 index <- which(as.Date(dates_of_variables, format = "%d/%m/%Y") == start_date_ext_reg)
                 column_data <- column_data[index:length(column_data)]

                 # create a dummy variable allowing to work with an MTS instead of a TS. Working with MTS allows to set the series names
                 if(j==1 && length(ts_list)==0)
                 {
                   dummy <- ts(data = column_data, class="ts", frequency = frequency, start = start_date)
                   ts_list <- list("dummy"=dummy)
                   mts_file_col_names <- c("dummy")
                 }

                 # Case of the 6 Trading Days variables in one file
                 if(user_def_var$n_var > 1)
                 {
                   column_name <- paste(column_name, "_", j, sep="")
                 }
                 mts_file_col_names <- c(mts_file_col_names, column_name)

                 time_series_new <- ts(column_data, start = start_date, frequency = frequency)

                 ts_list[[column_name]] <- time_series_new
               }
             }

             # DUBBI PARTONO QUI
             if(length(ts_list)>0)
             {
               mts_file <- do.call(ts.union, ts_list)

               colnames(mts_file) <- mts_file_col_names
               mts_file <- mts_file[,-1]
               if(all(is.na(mts_total)))
               {
                 mts_total <- mts_file#NA
               }else
               {
                 mts_total <- cbind(mts_total, mts_file)
               }
               mts_file<-NA
             }

             #browser()
             if(length(class(mts_total)=="ts")==1 && class(mts_total)=="ts")
             {
               #browser()
               data_ts <- as.vector(mts_total)
               dim(data_ts) <- c(length(mts_total), 1)
               dimnames(data_ts) <- list(NULL, column_name)
               mts_total <- ts(data = data_ts, start = start_date, frequency = frequency, class=c("mts", "ts", "matrix"), names=c(column_name))
             }


             return(mts_total)


           })
# read_ext_reg_info method, IT MUST RETURN A LIST of information on external regressors
#setGeneric("read_ext_reg_info", function(object, var_info_container, ...) standardGeneric("read_ext_reg_info"))
#' @export
setMethod ("read_ext_reg_info", signature("Data_reader_ext_reg_csv", "ANY"),
           function(object, var_info_container, ...) {

             workspace <- var_info_container

             jmodel <- RJDemetra::get_jmodel(workspace, progress_bar = TRUE) # to retrieve external regressors by name

             all_jmodel_vars <- getUserDefinedTdVariables_info_csv(jmodel) # per editare la scrittura
             return(all_jmodel_vars)

           })


# Definizione del costruttore R-like
#' @export
Data_reader_ext_reg_csv <- function(input_source, ...) {

  obj <- new("Data_reader_ext_reg_csv", input_source = input_source, read_ext_reg_data = function(...) read_ext_reg_data(obj, ...), read_ext_reg_info = function(...) read_ext_reg_info(obj,...),...)

  # Restituisci l'oggetto creato
  return(obj)
}


# Please use a different name for every data_reader_ext_reg, to avoid conflicts
# The variables values are available in files.
# input_mode = TS_regressor_file the start date is the same as the input data series
# input_mode = TODO JD_regressor_file file containing both dates and data
getUserDefinedTdVariables_info_csv <- function(jmodel ,input_mode=c("TS_regressor_file", "JD_regressor_file", ))
{
  var_info_list   = list()
  ramps_info_list = list()
  iv_info_list    = list()

  #browser()
  get_repeat_counts <- function(jUser_Td_VarsString) {
    # Controlla se jUser_Td_VarsString è una singola stringa
    if (is.character(jUser_Td_VarsString) && length(jUser_Td_VarsString) == 1) {
      return(1)  # Restituisce un vettore di un solo elemento con il valore 1
    } else {
      # Estrai il prefisso di ogni elemento
      prefixes <- gsub("^(.*)_\\d+$", "\\1", jUser_Td_VarsString)
      # Conta le ripetizioni dei prefissi
      repeat_counts <- table(prefixes)
      # Creazione del vettore di risultati
      result <- integer(length(prefixes))
      # Assegna il numero di ripetizioni per ogni variabile al vettore di risultati
      for (i in seq_along(prefixes)) {
        result[i] <- repeat_counts[prefixes[i]]
      }
      return(result)  # Restituisce un vettore con il numero di variabili ripetute
    }
  }
  repeated_vars <- c()

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
        file_name  <- paste0(file_name, ".csv")
        # Removes "r." o "R." from string prefix (r. automatically put when an RJDemetra workspace with external variables is created)
        #file_name <- gsub("^[rR]\\.", "", file_name)

        year       <- start(get_ts(jSA_series))[1]
        month      <- start(get_ts(jSA_series))[2]
        start_date <- as.Date(paste(year, month, "01", sep = "-"))

        start_date <- format(start_date, "%Y-%m-%d")
        current    <- list(container = file_name, start = start_date, n_var=1) #frequency=frequency(get_ts(jSA_series)), #, type="userdef")
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

      #browser()
      #i = length(var_info_list[[name]])+1 # if some usrDefVar have been already added, I start adding the TD user defined vars after them
      i       = idx_file_list
      repeated_vars <- get_repeat_counts(jUser_Td_VarsString)
      r_idx = 1 # repeated_vars index

      previous=list()
      for(varString in jUser_Td_VarsString)
      {
        file_name <- tolower(sub(".*\\.(.*?)_\\d+$", "\\1", varString))

        file_name <- paste0(file_name, ".csv")
        # Removes "r." o "R." from string prefix (r. automatically put when an RJDemetra workspace with external variables is created)
        #file_name <- gsub("^[rR]\\.", "", file_name)

        year  <- start(get_ts(jSA_series))[1]
        month <- start(get_ts(jSA_series))[2]
        start_date <- as.Date(paste(year, month, "01", sep = "-"))

        #browser()


        start_date <- format(start_date, "%Y-%m-%d")
        current = list(container = file_name, start = start_date, n_var=repeated_vars[r_idx]) ##frequency=frequency(get_ts(jSA_series)), # type="td")
        r_idx <- r_idx + 1
        if (!identical(current, previous))  # for the 6 variables case: all the variables belong to the same file, so the fileinfo will be replicated 6 time, but i save only one of them not incrementing i
        {
          file_list[[i]] = current
          previous       = file_list[[i]]
          i=i+1
        }
      }
    }

    #### Copy from here and the beginning if the function
    #browser()
    var_info_list[[name]] <- append(var_info_list[[name]], file_list)

    ramps_list   <- get_ramps(jSA_series)
    ramps_info_list[[name]] <- append(ramps_info_list[[name]], ramps_list)

    ivs_list     <- get_intervention_vars(jSA_series)
    iv_info_list[[name]] <- ivs_list

  }

  #browser()
  ret <- list("ext_vars"=var_info_list, "ramps"=ramps_info_list, "intervention_vars"=iv_info_list )

  return(ret)
}





find_file_case_insensitive <- function(directory, target_filename) {
  # Convert the target filename to lowercase for comparison
  target_lower <- tolower(target_filename)

  # List all files in the directory
  files <- list.files(directory)

  # Iterate through the files in the given directory
  for (file in files) {
    # Check if the lowercase version of the current file matches the target
    if (tolower(file) == target_lower) {
      # Return the matched filename
      return(file)
    }
  }

  # If no matching file is found, return an appropriate message
  return(NA)
}


# possible conflicts! leave these functions (get_ramps, get_intervention_vars) equals in all the scripts
# TODO: move in utility_functions or assign a different name in every Data_reader_ext_reg!
get_ramps<- function(series)
{
  ramps_ret <- list()

  regression<-series$spec$getRegression()
  core<-regression$getCore()
  core_regression<-core$getRegression()
  ramps_count<-core_regression$getRampsCount() # for loop over this

  i<-0
  while(i < ramps_count)
  {
    ramp_i       <-core_regression$getRamp(as.integer(i))
    begin_ramp_i <- ramp_i$getStart()$toString()
    end_ramp_i   <- ramp_i$getEnd()$toString()
    ramp_i_ret   <- list("start"=begin_ramp_i, "end"=end_ramp_i)

    ramps_ret[[length(ramps_ret) + 1]] <- ramp_i_ret

    i<-i+1
  }
  return(ramps_ret)


}

get_intervention_vars<- function(series)
{
  #browser()
  ivs_ret <- list()

  regression<-series$spec$getRegression()
  core<-regression$getCore()
  core_regression<-core$getRegression() #until here in common with RAMPS
  intervention_vars_count <- core_regression$getInterventionVariablesCount()
  i<-0

  while(i<intervention_vars_count)
  {
    int_var_i<-core_regression$getInterventionVariable(as.integer(i))
    int_var_i$getName()
    iv_i_delta     <- int_var_i$getDelta()
    iv_i_deltaS    <- int_var_i$getDeltaS()
    iv_i_sequences <- int_var_i$getSequences() #every IV could be created in different times thanks to multiple sequences associated
    n_seq <- iv_i_sequences$length #iterate over this
    j<-0
    seq_list <- list()
    while(j < n_seq)
    {
      iv_i_sequences_seq_j       <- int_var_i$getSequence(as.integer(j))
      iv_i_sequences_seq_j_start <- iv_i_sequences_seq_j$start$toString()
      iv_1_sequences_seq_j_end   <- iv_i_sequences_seq_j$end$toString()

      iv_1_sequences_seq_j_to_save <- list("start"=iv_i_sequences_seq_j_start, "end"= iv_1_sequences_seq_j_end)

      seq_list[[length(seq_list) + 1]] <- iv_1_sequences_seq_j_to_save
      j<-j+1
    }

    iv<- list("delta"=iv_i_delta, "delta_s"=iv_i_deltaS , "seq"=seq_list)
    ivs_ret[[length(ivs_ret) + 1]] <- iv

    i <- i+1
  }

  return(ivs_ret)

}

