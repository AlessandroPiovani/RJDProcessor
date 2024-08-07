#setwd("C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra_TEST_3\\")

# Carica il pacchetto RJDemetra
#install.packages("RJDemetra")
require(RJDemetra)
#install.packages("rjson")
require(rjson)

#source("utility_functions.R")
#source("Extended_tramoseats_spec.R")

#' @export
JD_JSON_to_virtual_workspace <- function(JSON_file, input_data_reader, ext_reg_data_reader=NA, series_to_proc_names=NA)
{

  wk <- new_workspace()
  new_multiprocessing(wk, "sa1")

  #browser()


  mts_input_time_series <- input_data_reader@read_data() #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  timestamps   <- rownames(mts_input_time_series)
  series_names <- colnames(mts_input_time_series) # sostituirlo con un vettore di serie che si vogliono destagionalizzare e aggiustare il codice nel for, per ora risolto con if nel for (che va anche bene)
  for (i in 1:ncol(mts_input_time_series)) {

    #browser()
    if(all(is.na(series_to_proc_names)) || series_names[i] %in% series_to_proc_names)
    {
      #browser()
      # Estrai la serie dalla matrice mts
      series <- mts_input_time_series[, i]

      # Trova l'indice del primo valore non-NA nella serie
      start_index <- which(!is.na(series))[1]

      # Estrai la parte della serie che inizia dal primo valore non-NA
      series <- gsub(",", ".", series)
      series_trimmed <- as.numeric(series[start_index:length(series)])

      # Calcola lo start basato sul primo valore non-NA
      start_date <- timestamps[start_index]

      # auto detection of frequency
      d1         <- as.Date(timestamps[start_index])
      d2         <- as.Date(timestamps[start_index+1])
      month_diff <- abs(as.numeric(format(d1, "%m")) - as.numeric(format(d2, "%m")))
      freq       <- 12/month_diff


      # Estrai l'anno e il mese dalla data
      year  <- as.integer(substr(start_date, 1, 4))  # Estrai i primi 4 caratteri (anno) e convertili in intero
      month <- as.integer(substr(start_date, 6, 7))  # Estrai i caratteri 6 e 7 (mese) e convertili in intero

      # Crea l'array start
      start <- c(year, month)

      #browser()


      # Costruisci l'oggetto ts
      ts_obj  <- ts(series_trimmed, start = start, frequency = freq)

      ts_name <- series_names[i]


      extended_tramoseats_spec_list <- read_spec_list_from_json_file(JSON_file, spec_format="list")
      extended_tramoseats_spec_obj  <- extended_tramoseats_spec_list[[ts_name]]


      tramoseats_spec_args <- to_tramoseats_spec_args(extended_tramoseats_spec_obj, ext_reg_data_reader)


      spec <- do.call(RJDemetra::tramoseats_spec, tramoseats_spec_args)
      #browser()
      sa <- tramoseats(ts_obj, spec = spec)


      add_sa_item(wk, "sa1", sa, ts_name)
    }


  }
  return(wk)

}



#' @export
JD_JSON_to_materialized_workspace <- function(workspace_dir=NA, JSON_file, input_data_reader, ext_reg_data_reader=NA, series_to_proc_names=NA)
{
  wk <- JD_JSON_to_virtual_workspace(JSON_file, input_data_reader, ext_reg_data_reader, series_to_proc_names)

  if(is.na(workspace_dir))
  {
    workspace_dir <- "output_workspace_container"
  }

  #browser()
  # Estrai il nome della directory
  workspace_dir <- basename(workspace_dir)

  # Verifica se il percorso contiene un percorso completo o solo il nome della directory
  if (dirname(workspace_dir) == ".") {
    # Se il percorso è solo il nome della directory, setta dir_path a getwd()
    dir_path <- getwd()
  } else {
    # Altrimenti, estrai il percorso della directory (esclusa la directory stessa)
    dir_path <- dirname(workspace_dir)
  }

  if (!file.exists(file.path(dir_path, workspace_dir))) {
    # Se non esiste, crea la directory in dir_path
    dir.create(file.path(dir_path, workspace_dir))
  }
  dir_path <- file.path(dir_path, workspace_dir)


  compute(wk)
  #model=get_model(wk)

  workspace_file_path <- file.path(dir_path, "workspace.xml")
  save_workspace(wk, workspace_file_path)

  return(wk) #
}



# e.g. workspace_xml_file_name = "C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra\\WorkspaceFAT-container\\"
#' @export
JD_JSON_from_materialized_workspace <- function(workspace_directory, ext_reg_input_data_reader, regr_directory=NA, JSON_file_name = "JD_JSON_specification.txt", diff=TRUE, java_processing = TRUE)
{
  ws<-load_workspace(file = workspace_directory)
  series_spec_list <- JD_JSON_from_virtual_workspace(ws, ext_reg_input_data_reader, JSON_file_name = JSON_file_name, diff=diff, java_processing = java_processing)
  #return(series_spec_list)
}



#' @export
JD_JSON_from_virtual_workspace <- function(ws, ext_reg_input_data_reader, JSON_file_name = "JD_JSON_specification.txt", diff=TRUE, java_processing=TRUE)
{
  compute(ws)

  series_spec_list  <-  extended_tramoseats_spec_list_from_workspace(workspace = ws, ext_reg_input_data_reader, java_processing=java_processing)

  #browser()

  # Apri il file in modalità scrittura
  con <- file(JSON_file_name, "w")

  writeLines("[\n", con, sep = "")
  n <- length(series_spec_list)

  # Itera attraverso ciascuna specifica nella lista, tranne l'ultima, che verrà stampata dopo il for, senza virgola finale
  for (i in seq_len(n - 1)) {
    # Ottieni la specifica corrente
    #browser()
    current_spec <- series_spec_list[[i]]

    # Converti la specifica in formato JSON
    json_spec <- to_JD_JSON(current_spec, indent = TRUE, diff=diff, basic_spec = current_spec@spec)

    # Scrivi la specifica nel file
    writeLines(json_spec, con, sep = ",\n\n\n")

  }

  # Ultima specifica senza "," finale
  current_spec <- series_spec_list[[n]]
  json_spec <- to_JD_JSON(current_spec, indent = TRUE, diff = diff)
  writeLines(json_spec, con, sep = "")

  writeLines("\n]", con, sep = "")


  writeLines("\n", con, sep = "") #per non avere errori in fase di rilettura, i file devono terminare per "\n"

  # Chiudi il file
  close(con)


  #file.copy(from = "specifications_new.txt", to = "specifications_old.txt", overwrite = TRUE)


  return(series_spec_list)
}

#' @export
from_reduced_to_full_JD_JSON_obj<-function(JD_JSON_string, basic_spec="RSA0")
{
  basic_spec   <- from_SA_spec(SA_spec = tramoseats_spec(basic_spec), userdef.varFromFile = FALSE)
  full_JD_JSON <- merge_objects_precedence_to_reduced(JD_JSON_obj, basic = basic_spec)
  return(full_JD_JSON)
}


merge_objects_precedence_to_reduced <- function(reduced, basic_spec = "RSA0")
{
  # Unisci gli oggetti con precedenza su reduced
  full_object <- basic_spec
  names(full_object) <- c(names(reduced), setdiff(names(basic_spec), names(reduced)))
  for (key in names(reduced)) {
    full_object[[key]] <- reduced[[key]]
  }

  return(full_object)
}



#' @export
from_reduced_to_full_JD_JSON_file <- function(JD_JSON_file, output_file_name=NA, indent= TRUE)
{
  # Definisci il nome del file
  if(is.na(output_file_name))
  {
    output_file_name <- gsub("\\.(\\w+)$", "_full.\\1", JD_JSON_file)

  }
  # Apri il file in modalità scrittura
  con <- file(output_file_name, "w")

  writeLines("[\n", con, sep = "")


  extended_tramoseats_spec_list <- read_spec_list_from_json_file(JD_JSON_file, spec_format="list")

  n <- length(extended_tramoseats_spec_list)

  for(i in seq_len(n - 1))
  {
    spec   <- extended_tramoseats_spec_list[[i]]

    #spec   <- convert_numerics_to_integers(spec) #metterlo nel costruttore?
    #browser()
    spec   <- do.call(Extended_tramoseats_spec, spec)

    json_spec <- to_JD_JSON(spec, indent = indent, diff = FALSE)
    writeLines(json_spec, con, sep = ",\n\n\n")
  }

  #browser()
  spec   <- extended_tramoseats_spec_list[[n]]
  #spec   <- convert_numerics_to_integers(spec)
  spec   <- do.call(Extended_tramoseats_spec, spec)

  json_spec <- to_JD_JSON(spec, indent = indent, diff = FALSE)
  writeLines(json_spec, con, sep = "")

  writeLines("\n]", con, sep = "")


  writeLines("\n", con, sep = "") #per non avere errori in fase di rilettura, i file devono terminare per "\n"

  # Chiudi il file
  close(con)
  closeAllConnections()

}



#' @export
from_full_to_reduced_JD_JSON_file<-function(JD_JSON_file, output_file_name=NA, indent= TRUE, basic_spec=NA)
{
  # Definisci il nome del file
  if(is.na(output_file_name))
  {
    output_file_name <- gsub("\\.(\\w+)$", "_reduced.\\1", JD_JSON_file)

  }
  # Apri il file in modalità scrittura
  con <- file(output_file_name, "w")

  writeLines("[\n", con, sep = "")

  #browser()
  extended_tramoseats_spec_list <- read_spec_list_from_json_file(JD_JSON_file, spec_format="list")

  n <- length(extended_tramoseats_spec_list)

  for(i in seq_len(n - 1))
  {
    #browser()

    spec   <- extended_tramoseats_spec_list[[i]]
    spec <- spec[!unlist(lapply(spec, function(x) all(is.na(x))))]

    #browser()
    if( is.na(basic_spec) )
    {
      basic_spec_f <- spec[["spec"]]
    } else
    {
      basic_spec_f <- basic_spec
    }


    #spec   <- convert_numerics_to_integers(spec) #metterlo nel costruttore?
    #browser()
    spec   <- do.call(Extended_tramoseats_spec, spec)

    json_spec <- to_JD_JSON(spec, indent = indent, diff = TRUE, basic_spec=basic_spec_f)
    writeLines(json_spec, con, sep = ",\n\n\n")
  }

  #browser()
  spec   <- extended_tramoseats_spec_list[[n]]
  spec   <- spec[!unlist(lapply(spec, function(x) all(is.na(x))))]

  if(is.na(basic_spec))
  {
    basic_spec_f <- spec[["spec"]]
  } else
  {
    basic_spec_f <- basic_spec
  }

  #spec   <- convert_numerics_to_integers(spec)
  spec   <- do.call(Extended_tramoseats_spec, spec)

  json_spec <- to_JD_JSON(spec, indent = indent, diff = TRUE, basic_spec=basic_spec_f)
  writeLines(json_spec, con, sep = "")

  writeLines("\n]", con, sep = "")


  writeLines("\n", con, sep = "") #per non avere errori in fase di rilettura, i file devono terminare per "\n"

  # Chiudi il file
  close(con)
  closeAllConnections()


}

