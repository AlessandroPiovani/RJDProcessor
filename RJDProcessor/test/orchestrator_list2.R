setwd("C:\\Users\\UTENTE\\Desktop\\RJDopenCruncher\\test")


library(RJDProcessor)



############################### Input defintion ################################
JD_JSON_file_full_reduced <- "specifications_new_full.txt"
input_workspace           <- "WorkspaceFAT-container\\WS-FAT.xml"
regr_directory            <- "SITIC-FAT\\regr\\"
spec_file_name            <- "specifications_new.txt"
load("input_list_FAT.RData") # object called input_list

diff <- TRUE # Reduced JSON if diff=TRUE, Full JSON forma

############################## Operational flow ################################

input_list<-input_list[1]
input_data_reader         <- Data_reader_list(input_source = input_list)

ext_reg_input_data_reader <- Data_reader_ext_reg_tsplus(regr_directory)


JD_JSON_from_materialized_workspace(input_workspace, ext_reg_input_data_reader, JSON_file_name = "specifications_new.txt", diff=TRUE, java_processing=FALSE)

series_to_proc_names <- c("FATEXP_10") # NA to process all the series #NA
virtual_workspace    <- JD_JSON_file_processor(input_data_reader = input_data_reader, ext_reg_data_reader = ext_reg_input_data_reader, spec_file_name = spec_file_name, output_workspace_dir = "output_workspace_container", series_to_proc_names = series_to_proc_names, java_processing = TRUE) # = NA) #output_workspace_dir can be omitted
                        # set java_processor=TRUE to speed-up the operations, but it does not work with workspaces readed by sa-ext plugin
m                    <- get_model(virtual_workspace) #get directly the R model (slower)

#m                   <- get_jmodel(virtual_workspace) # faster if you want to work in Java
#m                   <- get_r_model_from_j_model(m)

from_reduced_to_full_JD_JSON_file(spec_file_name)

from_full_to_reduced_JD_JSON_file(JD_JSON_file = JD_JSON_file_full_reduced)

compare_sa_ts(new_model_workspace = virtual_workspace, old_model_workspace = input_workspace , materialized_ws_new=FALSE, materialized_ws_old=TRUE, java_processing_old_model=FALSE)




