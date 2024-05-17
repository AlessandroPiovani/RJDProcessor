setwd("C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra_TEST_3\\")


#library(RJDemetra)
#library(rjson)

source("JD_JSON_file_processor.R")
source("JD_JSON.R")
source("Provider_csv_istat_format.R")
source("Provider_ext_reg_tsplus.R")


############################### Input defintion ################################
JD_JSON_file_full_reduced <- "C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra_TEST_3\\specifications_new_full.txt"

input_workspace_directory <- "C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra_TEST_3\\WorkspaceFAT-container\\WS-FAT.xml"
input_data_file_name      <- "C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra_TEST_3\\SITIC-FAT\\grezzi.csv"
regr_directory            <- "C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra_TEST_3\\SITIC-FAT\\regr"
# input_workspace_directory <- "C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra_TEST_3\\WorkspaceTUR-container\\workspace-TUR.xml"
# input_data_file_name      <- "C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra_TEST_3\\SITIC-TUR\\grezziTUR.csv"
# regr_directory            <- "C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra_TEST_3\\SITIC-TUR\\regr"

diff <- TRUE # Reduced JSON if diff=TRUE, Full JSON format otherwise

############################## Operational flow ################################

spec_file_name <- "specifications_new.txt"


input_data_provider    <- Provider_csv_istat_format(input_source = input_data_file_name)
ext_reg_input_provider <- Provider_ext_reg_tsplus(regr_directory)


JD_JSON_from_materialized_workspace(input_workspace_directory, ext_reg_input_provider, JSON_file_name = "specifications_new.txt", diff=TRUE)
  
series_to_proc_names <- NA #c("FATEXP_13", "C_DEFL", "FATEXP_14") # NA to process all the series #NA 
virtual_workspace    <- JD_JSON_file_processor(input_data_provider = input_data_provider, ext_reg_provider = ext_reg_input_provider, spec_file_name = spec_file_name, output_workspace_dir = "output_workspace_container" ,series_to_proc_names = series_to_proc_names) # = NA) #output_workspace_dir can be omitted
m                    <- get_model(virtual_workspace)
  
from_reduced_to_full_JD_JSON_file(spec_file_name)

from_full_to_reduced_JD_JSON_file(JD_JSON_file = JD_JSON_file_full_reduced)

