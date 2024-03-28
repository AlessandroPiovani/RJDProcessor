library(RJDBC)
library(RJDemetra)
library(rjson)


source("utility_functions.R")
source("Extended_tramoseats_spec.R")
source("basic_spec.R")
source("JD_JSON.R")

setwd("C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra_TEST_3\\")



input_data_file_name_and_path <- "C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra_TEST_2\\SITIC-FAT\\grezzi.csv"
regr_directory                <- "C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra_TEST_2\\SITIC-FAT\\regr"
models_specifications         <- "C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra_TEST_2\\specifications_new.txt"






drv <- JDBC("com.denodo.vdp.jdbc.Driver", "C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra_TEST_2\\denodo-vdp-jdbcdriver-8.0.2.jar")
url <- paste("jdbc:denodo://datavirtualization.sviluppo.istat.it:9999/", "sintesi", sep="")
conn <- dbConnect(drv, url, "alessandro.piovani", "Peterbill2024")
dbListTables(conn)
dbGetQuery(conn, paste("select count(*) from ", "desta_diretta"))
a<-dbGetQuery(conn, "select * from descr_combinazioni where id_indagine = 5")

# Esempio di chiamata pdella funzione che inserisce i modelli
str_query <- "SELECT p_return FROM sintesi.sp_carica_modelli() 
WHERE p_id_indagine=5
AND p_id_variabile='1000' 
AND p_id_combinazione='5' 
AND p_id_base='12' 
AND p_id_periodo_ini='202401' 
AND p_series_name='mia' 
AND p_specifications='{
\"series_name\":\"FATEXP_10\", 
\"spec\":\"RSA0\", 
\"transform.function\":\"Log\", 
\"usrdef.outliersEnabled\":true, 
\"arima.mu\":false
}'
  AND p_benchmark =NULL
  AND p_other =NULL"

res<-dbGetQuery(conn, str_query)


# per vedere se l'inserimento è andato a buon fine
#dbGetQuery(conn, paste("select * from ", "bv_dest_diretta")) # bv_dest_diretta è la tab fisica

# per vedere se l'inserimento è andato a buon fine
dbGetQuery(conn, paste("select * from ", "desta_diretta")) # desta_diretta è una vista del join fra bv_dest_diretta e bv_dest_catalogo




