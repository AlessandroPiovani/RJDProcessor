setwd("C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra_TEST_2\\")

# Carica il pacchetto RJDemetra
#install.packages("RJDemetra")
library(RJDemetra)
#install.packages("rjson")
library(rjson)

source("utility_functions.R")
source("Extended_tramoseats_spec.R")
source("basic_spec.R")

regr_directory <- "C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra_TEST_2\\SITIC-FAT\\regr"


################### Lettura da Workspace e scrittura su file ################### 

#ws<-load_workspace()
ws<-load_workspace(file = "C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra\\WorkspaceFAT-container\\WS-FAT.xml")
compute(ws)

series_spec_list  <-  extended_tramoseats_spec_list(workspace = ws, regr_directory = regr_directory)

#browser()
# Prova con una sola serie
# print("here")
# to_JD_JSON(series_spec_list[[1]], diff = TRUE)



# Definisci il nome del file
file_name <- "specifications_new.txt"
diff <- TRUE

# Apri il file in modalità scrittura
con <- file(file_name, "w")

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


file.copy(from = "specifications_new.txt", to = "specifications_old.txt", overwrite = TRUE)










