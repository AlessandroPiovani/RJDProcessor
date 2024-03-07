



workspace_to_JSON <- function(input_workspace_directory = NA, regr_directory = NA, spec_file_name = "specifications_new.txt", old_spec_file_name = NA, diff=TRUE)
{  

    
    source("utility_functions.R")
    source("Extended_tramoseats_spec.R")
    source("basic_spec.R")
    source("Data_reader_csv_istat_format.R")
    source("Data_reader_ext_reg_tsplus.R")
  
    
    require(RJDemetra)
    require(rjson)    
    
    ################### Lettura da Workspace e scrittura su file ################### 

    if(is.na(input_workspace_directory) )
    {
      ws<-load_workspace()
    }else
    {
      ws<-load_workspace(file = input_workspace_directory)
    }  
    
    compute(ws)
    
    series_spec_list  <-  extended_tramoseats_spec_list(workspace = ws, regr_directory = regr_directory)
    
    
    # Definisci il nome del file
    # spec_file_name <- "specifications_new.txt"
    

    # Apri il file in modalità scrittura
    con <- file(spec_file_name, "w")
    
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
    
    writeLines("\n]", con, sep = "") # chiude il JsonArray
    writeLines("\n", con, sep = "")  # per non avere errori in fase di rilettura, i file devono terminare per "\n"
    
    # Chiudi il file
    close(con)
    
    if(is.na(old_spec_file_name))
    {
      old_spec_file_name <- sub("\\.\\w+$", "_old\\0", spec_file_name)

        
    }  
    
    file.copy(from = spec_file_name, to = old_spec_file_name, overwrite = TRUE)
    


}





