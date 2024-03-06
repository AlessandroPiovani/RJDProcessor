setwd("C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra_TEST_2\\")

# Carica il pacchetto RJDemetra
#install.packages("RJDemetra")
require(RJDemetra)
#install.packages("rjson")
require(rjson)

source("utility_functions.R")
source("Extended_tramoseats_spec.R")


input_data_file_name_and_path <- "C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra\\SITIC-FAT\\grezzi.csv"
regr_directory                <- "C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra\\SITIC-FAT\\regr"
models_specifications         <- "C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra\\specifications.txt"





JD_JSON_to_virtual_workspace <- function(JSON_file, input_data_file, regr_directory=NA)
{
  
  wk <- new_workspace()
  new_multiprocessing(wk, "sa1")
  
  
  mts_input_time_series <- read_dati_grezzi(input_data_file)
  
  
  timestamps   <- rownames(mts_input_time_series)
  series_names <- colnames(mts_input_time_series) # sostituirlo con un vettore di serie che si vogliono destagionalizzare e aggiustare il codice nel for
  for (i in 1:ncol(mts_input_time_series)) 
  {
    # Estrai la serie dalla matrice mts
    series <- mts_input_time_series[, i]
    
    # Trova l'indice del primo valore non-NA nella serie
    start_index <- which(!is.na(series))[1]
    
    # Estrai la parte della serie che inizia dal primo valore non-NA
    series <- gsub(",", ".", series)
    series_trimmed <- as.numeric(series[start_index:length(series)])
    
    # Calcola lo start basato sul primo valore non-NA
    start_date <- timestamps[start_index]
    
    # Estrai l'anno e il mese dalla data
    year  <- as.integer(substr(start_date, 1, 4))  # Estrai i primi 4 caratteri (anno) e convertili in intero
    month <- as.integer(substr(start_date, 6, 7))  # Estrai i caratteri 6 e 7 (mese) e convertili in intero
    
    # Crea l'array start
    start <- c(year, month)
    
    # Costruisci l'oggetto ts
    ts_obj  <- ts(series_trimmed, start = start, frequency = 12)
    
    ts_name <- series_names[i]
    
    
    extended_tramoseats_spec_list <- read_spec_list_from_json_file(JSON_file, spec_format = "list")
    extended_tramoseats_spec_obj  <- extended_tramoseats_spec_list[[ts_name]]  
    
    
    tramoseats_spec_args <- to_tramoseats_spec_args(extended_tramoseats_spec_obj, regr_directory)
    
    
    spec <- do.call(RJDemetra::tramoseats_spec, tramoseats_spec_args)
    sa <- tramoseats(ts_obj, spec = spec)
    
    
    add_sa_item(wk, "sa1", sa, ts_name)
    
  }
  return(wk)
  
}  





JD_JSON_to_materialized_workspace <- function(JSON_file, input_data_file, regr_directory=NA, workspace_dir=NA)
{
  
  wk <- JD_JSON_to_virtual_workspace(JSON_file, input_data_file, regr_directory = regr_directory)
  
  if(is.na(workspace_dir))
  {
    workspace_dir <- "output_workspace_container"
    if (!file.exists(workspace_dir)) {
      # Crea la nuova cartella
      dir.create(workspace_dir)
    }
    
    # Ottieni il percorso completo della nuova cartella
    dir_path <- file.path(getwd(), workspace_dir)
    dir_path <- gsub("/", "\\\\", dir_path)
  }
  else
  {
    # Estrai il nome della directory
    workspace_dir <- basename(percorso)
    
    # Verifica se il percorso contiene un percorso completo o solo il nome della directory
    if (dirname(percorso) == ".") {
      # Se il percorso è solo il nome della directory, setta dir_path a getwd()
      dir_path <- getwd()
    } else {
      # Altrimenti, estrai il percorso della directory (esclusa la directory stessa)
      dir_path <- dirname(percorso)
    }
  }
    
  compute(wk)
  #model=get_model(wk)
  
  save_workspace(wk, file.path(dir_path, "workspace.xml"))
}  



# e.g. workspace_xml_file_name = "C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra\\WorkspaceFAT-container\\WS-FAT.xml"
JD_JSON_from_materialized_workspace <- function(workspace_xml_file_name, regr_directory=NA, JSON_file_name = "JD_JSON_specification.txt", diff=TRUE)
{
  ws<-load_workspace(file = workspace_xml_file_name)
  series_spec_list <- JD_JSON_from_virtual_workspace(ws, regr_directory=regr_directory, JSON_file_name = JSON_file_name, diff=diff)
  return(series_spec_list)  
}




JD_JSON_from_virtual_workspace <- function(ws, regr_directory=NA, JSON_file_name = "JD_JSON_specification.txt", diff=TRUE)
{
  compute(ws)
  
  series_spec_list  <-  extended_tramoseats_spec_list(workspace = ws, regr_directory = regr_directory)
  
  # Definisci il nome del file
  #file_name <- "specifications_new.txt"
  #diff <- TRUE
  
  # Apri il file in modalità scrittura
  con <- file(JSON_file_name, "w")
  
  writeLines("[\n", con, sep = "")
  n <- length(series_spec_list)
  
  # Itera attraverso ciascuna specifica nella lista, tranne l'ultima, che verrà stampata dopo il for, senza virgola finale
  for (i in seq_len(n - 1)) {
    # Ottieni la specifica corrente
    current_spec <- series_spec_list[[i]]
    
    # Converti la specifica in formato JSON
    json_spec <- to_JD_JSON(current_spec, indent = TRUE, diff=diff)
    
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


from_reduced_to_full_JD_JSON_obj<-function(JD_JSON_string, basic_spec="RSA_0")
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




from_full_to_reduced_JD_JSON_file<-function(JD_JSON_file, output_file_name=NA, indent= TRUE, basic_spec="RSA0")
{
  # Definisci il nome del file
  if(is.na(output_file_name))
  {
    output_file_name <- gsub("\\.(\\w+)$", "_reduced.\\1", JD_JSON_file)
    
  }  
  # Apri il file in modalità scrittura
  con <- file(output_file_name, "w")
  
  writeLines("[\n", con, sep = "")
  
  
  extended_tramoseats_spec_list <- read_spec_list_from_json_file(JD_JSON_file, spec_format="list")
  
  n <- length(extended_tramoseats_spec_list)
  
  for(i in seq_len(n - 1))
  {
    #browser()
    
    spec   <- extended_tramoseats_spec_list[[i]]
    #spec   <- spec[!unlist(lapply(spec, is.na))]     # Remove NA elements. It is important because Extended_tramoseats_spec wants typed NA insetead of generic NA present in spec read from JSON, so it returns an error 
    #spec <- spec[!unlist(lapply(spec, function(x) is.na(x) || identical(x, NA)))]
    spec <- spec[!unlist(lapply(spec, function(x) all(is.na(x))))]
    

    
    #spec   <- convert_numerics_to_integers(spec) #metterlo nel costruttore?
    #browser()
    spec   <- do.call(Extended_tramoseats_spec, spec)
    
    json_spec <- to_JD_JSON(spec, indent = indent, diff = TRUE, basic_spec=basic_spec)
    writeLines(json_spec, con, sep = ",\n\n\n")
  }  
  
  #browser()
  spec   <- extended_tramoseats_spec_list[[n]]
  spec   <- spec[!unlist(lapply(spec, function(x) all(is.na(x))))]
  
  #spec   <- convert_numerics_to_integers(spec)
  spec   <- do.call(Extended_tramoseats_spec, spec)
  #browser()
  json_spec <- to_JD_JSON(spec, indent = indent, diff = TRUE, basic_spec=basic_spec)
  writeLines(json_spec, con, sep = "")
  
  writeLines("\n]", con, sep = "")
  
  
  writeLines("\n", con, sep = "") #per non avere errori in fase di rilettura, i file devono terminare per "\n"
  
  # Chiudi il file
  close(con)
  closeAllConnections()
  
  
}


