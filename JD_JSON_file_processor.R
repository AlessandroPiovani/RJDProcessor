setwd("C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra_TEST_2\\")

# Carica il pacchetto RJDemetra
#install.packages("RJDemetra")
library(RJDemetra)
#install.packages("rjson")
library(rjson)

source("utility_functions.R")
source("Extended_tramoseats_spec.R")
source("basic_spec.R")



input_data_file_name_and_path <- "C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra_TEST_2\\SITIC-FAT\\grezzi.csv"
regr_directory                <- "C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra_TEST_2\\SITIC-FAT\\regr"
models_specifications         <- "C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra_TEST_2\\specifications_new.txt"

wk <- new_workspace()
new_multiprocessing(wk, "sa1")



mts_input_time_series <- read_dati_grezzi(input_data_file_name_and_path)


timestamps   <- rownames(mts_input_time_series)
series_names <- colnames(mts_input_time_series) # sostituirlo con un vettore di serie che si vogliono destagionalizzare e aggiustare il codice nel for
for (i in 1:ncol(mts_input_time_series)) {
  
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

  
  extended_tramoseats_spec_list <- read_spec_list_from_json_file(models_specifications, spec_format="list")
  extended_tramoseats_spec_obj  <- extended_tramoseats_spec_list[[ts_name]]  
  
  
  tramoseats_spec_args <- to_tramoseats_spec_args(extended_tramoseats_spec_obj, regr_directory)
  

  spec <- do.call(RJDemetra::tramoseats_spec, tramoseats_spec_args)
  sa <- tramoseats(ts_obj, spec = spec)
  
  
  add_sa_item(wk, "sa1", sa, ts_name)
  
  
  
}

# browser()

workspace_dir <- "output_workspace_container"
if (!file.exists(workspace_dir)) {
  # Crea la nuova cartella
  dir.create(workspace_dir)
}

# Ottieni il percorso completo della nuova cartella
dir_path <- file.path(getwd(), workspace_dir)
dir_path <- gsub("/", "\\\\", dir_path)

compute(wk)
model=get_model(wk)

save_workspace(wk, file.path(dir_path, "workspace.xml"))


zz <- file("elaborazione.out", open="wt")
sink(zz, type)

print(model)

sink(file=NULL)

i=1
nomi_serie = names(model[[1]])

pdf(file="plots.pdf")

for(serie in model[[1]])
{
  #print(nomi_serie[i])
  plot.new()
  text(x=.5, y=.5, nomi_serie[i], cex=2)  # first 2 numbers are xy-coordinates within [0, 1]
  plot(serie, type_chart = "sa-trend")
  plot(serie$decomposition)
  i=i+1
}  

dev.off()
close.connection(zz)







