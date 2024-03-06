setwd("C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra_TEST_2\\")

# Carica il pacchetto RJDemetra
#install.packages("RJDemetra")
require(RJDemetra)
#install.packages("rjson")
require(rjson)

source("utility_functions.R")
source("Extended_tramoseats_spec.R")
source("JD_JSON.R")


JD_JSON_file <- "C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra_TEST_2\\specifications_new_full.txt"

from_full_to_reduced_JD_JSON_file(JD_JSON_file)


  