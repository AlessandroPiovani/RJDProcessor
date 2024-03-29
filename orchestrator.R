setwd("C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra_TEST_3\\")


library(RJDemetra)
library(rjson)

source("Extended_tramoseats_spec.R")
source("utility_functions.R")
source("basic_spec.R")
#source("workspace_to_json.R")
source("JD_JSON_file_processor.R")
source("JD_JSON.R")

source("Data_reader_ext_reg_tsplus2.R")


############################### Input defintion ################################

input_workspace_directory <- "C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra_TEST_3\\WorkspaceFAT-container\\WS-FAT.xml"
regr_directory            <- "C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra_TEST_3\\SITIC-FAT\\regr"
input_data_file_name      <- "C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra_TEST_3\\SITIC-FAT\\grezzi.csv"
#JD_JSON_file_reduced      <- "C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra_TEST_3\\specifications_new.txt" # The spec_file_name will be used
JD_JSON_file_full_reduced <- "C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra_TEST_3\\specifications_new_full.txt"

diff <- TRUE # Reduced JSON if diff=TRUE, Full JSON format otherwise

############################## Operational flow ################################

spec_file_name <- "specifications_new.txt"


ext_reg_input_provider = Data_reader_ext_reg2(regr_directory)
JD_JSON_from_materialized_workspace(input_workspace_directory, ext_reg_input_provider, JSON_file_name = "specifications_new.txt", diff=TRUE)
  
series_to_proc_names <- c("FATEXP_13", "C_DEFL", "FATEXP_14") # NA to process all the series
input_provider = Data_reader(input_source = input_data_file_name)
model <- JD_JSON_file_processor(input_provider = input_provider, ext_var_provider = ext_reg_input_provider, spec_file_name = spec_file_name, output_workspace_dir = "output_workspace_container" ,series_to_proc_names = series_to_proc_names) #output_workspace_dir can be omitted
  
from_reduced_to_full_JD_JSON_file(spec_file_name)

from_full_to_reduced_JD_JSON_file(JD_JSON_file = JD_JSON_file_full_reduced)

  