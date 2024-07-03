# Author:      Piovani Alessandro
# Contact:     alessandro.piovani@istat.it
# Description: functions to auto comment elaborazione.out results,
#              highlighting differences between old and new models

library(stringr)

extract_outlier_parts <- function(input_string) {
  # Definisci il pattern regolare per estrarre le parti
  pattern <- "([A-Z]+)([0-9]+)\\(([0-9]+), (-?[0-9]+\\.?[0-9]*)\\);?"
  
  # Usa str_match per estrarre le parti
  matches <- str_match(input_string, pattern)
  
  # Inizializza un dizionario vuoto
  result <- list()
  
  # Estrai le parti individuate e assegnale al dizionario
  if (!is.null(matches)) {
    result$tipo <- matches[1, 2]
    result$numero <- matches[1, 3]
    result$data <- matches[1, 4]
    result$valore <- matches[1, 5]
  } else {
    cat("Nessuna corrispondenza trovata.\n")
  }
  
  return(result)
}

create_all_series_outlier_dictionary <- function(file="oldt/toutlier.m") {
  # Leggi il file "newt/toutlier.m"
  toutlier_data <- readLines(file)
  
  # Inizializza una lista vuota per i dati
  toutlier_list <- list()
  
  # Salta la prima riga
  toutlier_data <- toutlier_data[-1]
  
  # Itera attraverso le righe rimanenti
  for (line in toutlier_data) {
    # Tokenizza la riga separando gli elementi per spazi e tab
    tokens <- unlist(str_split(line, "\\s+"))
    nome_serie <- gsub("\"", "", tokens[3])
    
    # Estrai la parte "outlier" dalla riga
    outlier_string <- paste(tokens[4:length(tokens)], collapse = " ") # Unisci i token dall'indice 4 in poi
    
    # Divide la stringa degli outlier in singoli outlier
    outlier_parts <- unlist(str_extract_all(outlier_string, "[A-Z]+[0-9]+\\([^)]+\\);"))
    
    # Crea una lista per gli outlier
    outlier_list <- list()
    
    # Estrai ciascun outlier e aggiungilo alla lista
    for (single_outlier in outlier_parts) {
      outlier_name <- sub("\\(.*$", "", single_outlier)
      outlier_name <- substr(outlier_name[[1]],1 ,2)
      outlier_data <- sub("^[A-Z]+[0-9]+\\(", "", single_outlier)
      outlier_data <- sub("\\);$", "", outlier_data)
      # Crea la chiave nel formato "TIPO_DATA"
      outlier_key <- paste(outlier_name, outlier_data, sep = "_")
      outlier_key <- strsplit(outlier_key, ",")[[1]][1]
      
      outlier_list[[outlier_key]] <- single_outlier
    }
    
    # Crea un dizionario con i campi desiderati
    # toutlier_entry <- list(
    #   nome_serie = outlier_list
    # )
    #toutlier_list <- append(toutlier_list, toutlier_entry)  # Aggiungi l'elemento alla lista toutlier_entry
    
    toutlier_list[[nome_serie]] <- outlier_list
    # Aggiungi il dizionario all'elenco
    # toutlier_list <- c(toutlier_list, toutlier_entry)
  }
  
  return(toutlier_list)
}


print_dictionary_values <- function(list) {
  values <- unlist(list)
  result <- paste(values, collapse = "  ")
  return(result)
}


# Funzione per confrontare i campi chiave dei dizionari
compare_dicts <- function(old_dict, new_dict) {
  output <- ""
  
  # Controlla i campi chiave del primo dizionario
  for (key in names(old_dict)) {
    if (!(key %in% names(new_dict))) {
      output <- paste0(output, "-", old_dict[[key]], " ")
    }
  }
  
  # Controlla i campi chiave del secondo dizionario
  for (key in names(new_dict)) {
    if (!(key %in% names(old_dict))) {
      output <- paste0(output, "+", new_dict[[key]], " ")
    }
  }
  
  return(output)
}


read_tfit_file <- function(filename) {
  file_content <- readLines(filename)
  file_content <- file_content[-1] # togli prima riga con info Tramo-Seats
  
  # Estrapola i nomi delle colonne dalla seconda riga (ora prima riga)
  column_names <- unlist(strsplit(file_content[1], "\\s+"))
  column_names <- column_names[column_names != ""]  # Rimuovi gli elementi vuoti dalla lista
  
  
  # Crea un nuovo dataframe vuoto
  #data <- data.frame(matrix(nrow = length(file_content) - 2, ncol = length(column_names)))
  data <- data.frame(matrix(ncol = length(column_names)))
  
  
  # Assegna i nomi alle colonne del dataframe
  colnames(data) <- column_names
  
  # Riempi il dataframe con i valori dal file
  j=1
  for (i in 1:length(file_content)) {
    row_values <- unlist(strsplit(file_content[i], "\\s+"))
    row_values <- row_values[2:length(row_values)] # il primo valore è sempre "", lo tolgo
    # data[i - 2, ] <- as.numeric(row_values) #TITLE non viene settato perchè è l'unico non numerico
    
    if (!is.na(suppressWarnings(as.numeric(row_values[1])))) # se non sono nell'header o nella stringa di descriozione di TS
    {  
      data[j, ] <- row_values
      j = j+1
    }
  }
  
  # Ottengo tutte le colonne tranne "TITLE", e le converto in numeri
  cols_to_convert <- setdiff(names(data), "TITLE")
  data[cols_to_convert] <- sapply(data[cols_to_convert], as.numeric)
  
  
  
  # if(! is.na(as.numeric(matchvect[2])))
  # {
  #   data[j, "n"]     <- as.numeric(matchvect[2])
  #   data[j, "TITLE"] <- matchvect[3]
  #   
  #   if (substr(matchvect[4], 1, 1) == "-") # per indentazione aggiungo 7 spazi alle diagnostiche che iniziano con -
  #   {
  #     matchvect[4] <- paste0("       ", matchvect[4])
  #   }
  #   data[j, "params_string"] <- matchvect[4]
  #   
  #   
  #   j = j+1
  # }
  
  
  return(data)
}


read_diagnostic_file <- function(filename) {
  file_content <- readLines(filename)
  #file_content <- file_content[-2] # togli prima riga con info Tramo-Seats e l'header
  
  # Estrapola i nomi delle colonne dalla seconda riga (ora prima riga)
  #column_names <- unlist(strsplit(file_content[1], "\\s+"))
  #column_names <- column_names[column_names != ""]  # Rimuovi gli elementi vuoti dalla lista
  column_names <- c("n", "TITLE", "params_string")
  
  # Crea un nuovo dataframe vuoto
  # data <- data.frame(matrix(nrow = length(file_content), ncol = 3)) # 3 colonne: n, TITLE, params_string
  data <- data.frame(matrix(ncol = 3))
  
  # Assegna i nomi alle colonne del dataframe
  colnames(data) <- column_names
  
  j = 1
  # Riempi il dataframe con i valori dal file
  for (i in 1:length(file_content)) {
    
    match <- regexec("^\\s*\"*\\s*([0-9]+)\\s+\"([^\"]+)\"\\s+(.*)$", file_content[i])
    matchvect <- regmatches(file_content[i], match)[[1]]
    match_str <- matchvect[1]
    
    if(! is.na(as.numeric(matchvect[2])))
    {
      data[j, "n"]     <- as.numeric(matchvect[2])
      data[j, "TITLE"] <- matchvect[3]
      
      if (substr(matchvect[4], 1, 1) == "-") # per indentazione aggiungo 7 spazi alle diagnostiche che iniziano con -
      {
        matchvect[4] <- paste0("       ", matchvect[4])
      }
      data[j, "params_string"] <- matchvect[4]
      
      
      j = j+1
    }
  }
  
  return(data)
}



get_EE <- function(tcalend_row) {
  # Dividi la stringa in pezzi utilizzando le parentesi chiuse come delimitatori
  pezzi <- unlist(strsplit(tcalend_row, "\\)"))
  
  # Estrai l'ultimo elemento (che è il testo tra l'ultima parentesi e la fine)
  testo_tra_le_parentesi <- tail(pezzi, n = 1)[1]
  
  # Aggiungi l'ultima parentesi alla fine del testo
  testo_tra_le_parentesi <- paste0(testo_tra_le_parentesi, ")")
  
  return(testo_tra_le_parentesi)
}


extract_regressors_from_string <- function(params_string) {
  # Inizializza una lista vuota per i regressori
  regressori <- c()
  
  # Pattern regex per individuare i regressori
  pattern <- "Reg\\d{2}\\(.*?\\);"
  
  # Estrai i regressori dalla stringa params_string
  regressori_row <- regmatches(params_string, gregexpr(pattern, params_string))
  if (length(regressori_row[[1]]) > 0) {
    regressori <- c(regressori,regressori_row[[1]])
  }
  return(regressori)
  
  
  
}


extract_fit_from_string <- function(params_string) {
  # Suddividi la stringa in elementi utilizzando spazi o tabulazioni come delimitatori
  params <- unlist(strsplit(params_string, "\\s+"))
  
  # Estrai il penultimo campo
  penultimate_field <- params[length(params) - 1]
  
  return(penultimate_field)
}
