setwd("C://Users//UTENTE//OneDrive - ISTAT//Desktop//RJDopenCruncher//RJDProcessor//test")

# library(RJDProcessor) # commented for tests

library(RJDemetra)


# Interfaces for Data_reader and Data_reader_ext_reg
setGeneric("read_data", function(object, ...) standardGeneric("read_data"))
setGeneric("read_ext_reg_data", function(object, var_info=NULL, time_series_info=NULL, frequency= NA_integer_, ...) standardGeneric("read_ext_reg_data"))
setGeneric("read_ext_reg_info", function(object, var_info_container, adjust_path=TRUE, ...) standardGeneric("read_ext_reg_info"))
source("C://Users//UTENTE//OneDrive - ISTAT//Desktop//RJDopenCruncher//RJDProcessor//R//Data_reader_xlsx.R")
source("C://Users//UTENTE//OneDrive - ISTAT//Desktop//RJDopenCruncher//RJDProcessor//R//Data_reader_csv.R")
source("C://Users//UTENTE//OneDrive - ISTAT//Desktop//RJDopenCruncher//RJDProcessor//R//Data_reader_list.R")
source("C://Users//UTENTE//OneDrive - ISTAT//Desktop//RJDopenCruncher//RJDProcessor//R//Data_reader_xml.R")
source("C://Users//UTENTE//OneDrive - ISTAT//Desktop//RJDopenCruncher//RJDProcessor//R//Data_reader_csv_istat_format.R")
source("C://Users//UTENTE//OneDrive - ISTAT//Desktop//RJDopenCruncher//RJDProcessor//R//Data_reader_ext_reg_xlsx.R")
source("C://Users//UTENTE//OneDrive - ISTAT//Desktop//RJDopenCruncher//RJDProcessor//R//Data_reader_ext_reg_tsplus.R")
source("C://Users//UTENTE//OneDrive - ISTAT//Desktop//RJDopenCruncher//RJDProcessor//R//Data_reader_ext_reg_csv.R")

source("C://Users//UTENTE//OneDrive - ISTAT//Desktop//RJDopenCruncher//RJDProcessor//R//utility_functions.R")
source("C://Users//UTENTE//OneDrive - ISTAT//Desktop//RJDopenCruncher//RJDProcessor//R//basic_spec.R")
source("C://Users//UTENTE//OneDrive - ISTAT//Desktop//RJDopenCruncher//RJDProcessor//R//Extended_tramoseats_spec.R")
source("C://Users//UTENTE//OneDrive - ISTAT//Desktop//RJDopenCruncher//RJDProcessor//R//JD_JSON.R")
source("C://Users//UTENTE//OneDrive - ISTAT//Desktop//RJDopenCruncher//RJDProcessor//R//JD_JSON_file_processor.R")
source("C://Users//UTENTE//OneDrive - ISTAT//Desktop//RJDopenCruncher//RJDProcessor//R//workspaces_manager.R")



############################### Input defintion ################################
JD_JSON_file_full_reduced <- "specifications_new_full.txt"

spec_file_name            <- "specifications_new.txt"


input_workspace      <- "WorkspaceFAS-standard-container\\FAS.xml"
input_data_file_name <- "CSV-FAS\\grezzi_trim_FAS.csv"
regr_directory       <- "CSV-FAS\\regr"

diff <- TRUE # Reduced JSON if diff=TRUE, Full JSON format otherwise

############################## Operational flow ################################



input_data_reader         <- Data_reader_csv(input_source = input_data_file_name)

ext_reg_input_data_reader <- Data_reader_ext_reg_csv(regr_directory)


JD_JSON_from_materialized_workspace(input_workspace, ext_reg_input_data_reader, JSON_file_name = "specifications_new.txt", diff=TRUE, java_processing=FALSE)

series_to_proc_names <- NA #c("FATEXP_13", "C_DEFL", "FATEXP_14") # NA to process all the series #NA
virtual_workspace    <- JD_JSON_file_processor(input_data_reader = input_data_reader, ext_reg_data_reader = ext_reg_input_data_reader, spec_file_name = spec_file_name, output_workspace_dir = "output_workspace_container", series_to_proc_names = series_to_proc_names, java_processing = TRUE) # = NA) #output_workspace_dir can be omitted
# set java_processor=TRUE to speed-up the operations, but it does not work with workspaces readed by sa-ext plugin
m                    <- get_model(virtual_workspace) #get directly the R model (slower)

#m                   <- get_jmodel(virtual_workspace) # faster if you want to work in Java
#m                   <- get_r_model_from_j_model(m)

from_reduced_to_full_JD_JSON_file(spec_file_name)

from_full_to_reduced_JD_JSON_file(JD_JSON_file = JD_JSON_file_full_reduced)

compare_sa_ts(new_model_workspace = virtual_workspace, old_model_workspace = input_workspace, materialized_ws_new=FALSE, materialized_ws_old=TRUE, java_processing_old_model=FALSE)




