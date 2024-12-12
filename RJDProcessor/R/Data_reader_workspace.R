
# input_source <- "C:/Users/UTENTE/Desktop/RJDopenCruncher/RJDProcessor/test/Workspace_from_scratch_container/Workspace_from_scratch.xml"
#
# read_data <- function(input_source) {
#   browser()
#   # setwd("C:/Users/UTENTE/Desktop/RJDopenCruncher/RJDProcessor")
#   # setwd("test/")
#
#   ws <- load_workspace(input_source)
#   compute(ws)
#   jmodel <- get_jmodel(ws)
#
#   all_series <- list()
#   series_names <- character()
#
#   for (multiprocessing in jmodel) {
#     for (series_name in names(multiprocessing)) {
#       time_series <- multiprocessing[[series_name]]
#       ts <- get_indicators(x = time_series, "y")
#       all_series[[series_name]] <- ts
#       series_names <- c(series_names, series_name)
#     }
#   }
#
#   # Convert the list of time series to a matrix of time series (mts)
#   mts <- do.call(cbind, all_series)
#
#   # Set the column names of the mts object
#   colnames(mts) <- series_names
#
#   return(mts)
# }
#
# read_data(input_source)






# Definition of S4 class
setClass("Data_reader_workspace",
         slots = list(
           input_source     = "ANY",
           read_data        = "function",
           ...              = "ANY"
         ))


#' Get the data from a Data_reader_workspace
#'
#' This function returns the data from the input_source of the object.
#'
#' @return data in form of numeric matrix, with rownames = dates (in string format, YYYY-MM-DD) and colnames = time series names (string)
#' @examples
#' input_data_file_name <- system.file("extdata","WorkspaceTUR-container/workspace-TUR.xml", package = "RJDProcessor")
#' input_data_reader    <- Data_reader_workspace(input_source = input_data_file_name)
#' mts <- input_data_reader@read_data()
#' @export
setMethod ("read_data", signature("Data_reader_workspace"),
           function(object, ...) {
             require("RJDemetra")

             #browser()
             #setwd("C:/Users/UTENTE/Desktop/RJDopenCruncher/RJDProcessor")
             #setwd("test/")

             ws     <- load_workspace(object@input_source)
             compute(ws)
             jmodel <- get_jmodel(ws)

             all_series <- list()
             series_names <- character()

             for (multiprocessing in jmodel) {
               for (series_name in names(multiprocessing)) {
                 time_series <- multiprocessing[[series_name]]
                 ts <- get_indicators(x = time_series, "y")
                 all_series[[series_name]] <- ts
                 series_names <- c(series_names, series_name)
               }
             }

             # Convert the list of time series to a matrix of time series (mts)
             # mts <- do.call(cbind, all_series)
             #
             # # Set the column names of the mts object
             # colnames(mts) <- series_names
             #
             # browser()
             # return(mts)
             #return(all_series)

             list_to_mts(all_series)

           })

#' Constructor (R-like) of the Data_reader object
#'
#' This function creates a Data_reader object capable of reading data from
#' workspaces and returning them using the \code{read_data()} function.
#'
#' @param input_source A string with workspace xml file name (also with path).
#' @return The Data_reader_workspace object
#' @examples
#' input_data_file_name <- system.file("extdata","WorkspaceTUR-container/workspace-TUR.xml", package = "RJDProcessor")
#' input_data_reader    <- Data_reader_workspace(input_source = input_data_file_name)
#' mts <- input_data_reader@read_data()
#' @export
Data_reader_workspace <- function(input_source = NA, ...) {
  require(RJDemetra)

  obj <- new("Data_reader_workspace", input_source = input_source, read_data = function() read_data(obj))

  return(obj)
}



list_to_mts <- function(ts_list) {

  # Frequenza della serie temporale (tutte le serie hanno la stessa frequenza)
  freq <- frequency(ts_list[[1]]$y)

  series_names <- names(ts_list)

  # Funzione per convertire anno e mese in un numero seriale
  date_to_numeric <- function(date) {
    return(date[1] * 12 + date[2] * 12/freq)
  }

  # Estrazione delle date di inizio e fine di tutte le serie
  start_dates <- sapply(ts_list, function(ts) start(ts$y))
  end_dates   <- sapply(ts_list, function(ts) end(ts$y))

  # Aggiustare i mesi per frequenza trimestrale
  if(freq == 4) {
    start_dates[2,] <- ((start_dates[2,]-1) * 3) + 1
    end_dates[2,]   <- ((end_dates[2,]-1) * 3) + 1
  }

  # Convertire le date in numeri seriali
  start_numerics <- sapply(1:ncol(start_dates), function(i) date_to_numeric(start_dates[,i]))
  end_numerics   <- sapply(1:ncol(end_dates), function(i) date_to_numeric(end_dates[,i]))

  # Trovare l'indice del numero seriale minimo e massimo
  min_start_index <- which.min(start_numerics)
  max_end_index   <- which.max(end_numerics)

  # Ricostruire le date di inizio e fine
  start_all_array <- start_dates[, min_start_index]
  end_all_array   <- end_dates[, max_end_index]

  # Costruire le stringhe delle date di inizio e fine
  start_all_date <- paste(start_all_array[1], start_all_array[2], "01", sep="-")
  end_all_date   <- paste(end_all_array[1], end_all_array[2], "01", sep="-")

  # Creazione della sequenza di date tra start_all e end_all
  if (freq == 12) {
    # Mensile
    all_dates <- seq.Date(from = as.Date(start_all_date),
                          to = as.Date(end_all_date),
                          by = "month")
  } else if (freq == 4) {
    # Trimestrale
    all_dates <- seq.Date(from = as.Date(start_all_date),
                          to = as.Date(end_all_date),
                          by = "quarter")
  }

  # Creazione della lista per contenere le serie temporali allineate
  mts_list <- vector("list", length(ts_list))

  for (i in seq_along(ts_list)) {
    ts <- ts_list[[i]]$y
    ts_dates <- seq.Date(from = as.Date(paste(start(ts)[1], start(ts)[2], "01", sep = "-")),
                         by = ifelse(freq == 12, "month", "quarter"),
                         length.out = length(ts))

    # Allineamento della serie con la sequenza di date
    ts_aligned <- rep(NA, length(all_dates))
    match_dates <- match(ts_dates, all_dates)
    #browser()
    ts_aligned[match_dates] <- as.vector(ts)

    mts_list[[i]] <- ts_aligned
  }

  mts <- ts(do.call(cbind, mts_list), start = c(start_all_array[1], start_all_array[2]), frequency = freq)

  colnames(mts) <- series_names

  all_dates <- as.character(all_dates)
  rownames(mts) <- all_dates

  return(mts)
}


