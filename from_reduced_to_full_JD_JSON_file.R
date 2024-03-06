setwd("C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra_TEST_2\\")

# Carica il pacchetto RJDemetra
#install.packages("RJDemetra")
require(RJDemetra)
#install.packages("rjson")
require(rjson)

source("utility_functions.R")
source("Extended_tramoseats_spec.R")
source("JD_JSON.R")


JD_JSON_file <- "C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra_TEST_2\\specifications_new.txt"

from_reduced_to_full_JD_JSON_file(JD_JSON_file)


  