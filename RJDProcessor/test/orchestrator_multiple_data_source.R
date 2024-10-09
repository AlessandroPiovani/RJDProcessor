setwd("C:\\Users\\UTENTE\\Desktop\\RJDopenCruncher\\test")


library("RJDProcessor")
library("RJDemetra")



############################### Input defintion ################################
JD_JSON_file_full_reduced <- "specifications_new_full.txt"

spec_file_name_xlsx            <- "specifications_new_xlsx.txt"
spec_file_name_csv             <- "specifications_new_csv.txt"


input_workspace_directory <- "WorkspaceFAS-standard-container\\FAS.xml"
input_data_file_name_xlsx <- "XLSX-FAS\\grezzi_trim_FAS.xlsx"
input_data_file_name_csv  <- "CSV-FAS\\grezzi_trim_FAS.csv"

regr_directory_xlsx       <- "XLSX-FAS\\regr"
regr_directory_csv        <- "CSV-FAS\\regr"



diff <- TRUE # Reduced JSON if diff=TRUE, Full JSON format otherwise

############################## Operational flow ################################



input_data_reader_xlsx         <- Data_reader_xlsx(input_source = input_data_file_name_xlsx)
input_data_reader_csv          <- Data_reader_csv(input_source  = input_data_file_name_csv)



ext_reg_input_data_reader_xlsx <- Data_reader_ext_reg_xlsx(regr_directory_xlsx)
ext_reg_input_data_reader_csv  <- Data_reader_ext_reg_csv(regr_directory_csv)


JD_JSON_from_materialized_workspace(input_workspace_directory, ext_reg_input_data_reader_xlsx, JSON_file_name = "specifications_new_xlsx.txt", diff=TRUE, java_processing=FALSE)
JD_JSON_from_materialized_workspace(input_workspace_directory, ext_reg_input_data_reader_csv,  JSON_file_name = "specifications_new_csv.txt",  diff=TRUE, java_processing=FALSE)


series_to_proc_names <- NA #c("FATEXP_13", "C_DEFL", "FATEXP_14") # NA to process all the series #NA 
virtual_workspace_csv    <- JD_JSON_file_processor(input_data_reader = input_data_reader_csv, ext_reg_data_reader  = ext_reg_input_data_reader_csv , spec_file_name = spec_file_name_csv,  output_workspace_dir = "output_workspace_container_csv",  series_to_proc_names = series_to_proc_names, java_processing = TRUE) # = NA) #output_workspace_dir can be omitted
virtual_workspace_xlsx   <- JD_JSON_file_processor(input_data_reader = input_data_reader_xlsx, ext_reg_data_reader = ext_reg_input_data_reader_xlsx, spec_file_name = spec_file_name_xlsx, output_workspace_dir = "output_workspace_container_xlsx", series_to_proc_names = series_to_proc_names, java_processing = TRUE) # = NA) #output_workspace_dir can be omitted

# set java_processor=TRUE to speed-up the operations, but it does not work with workspaces readed by sa-ext plugin
m                    <- get_model(virtual_workspace_csv) #get directly the R model (slower)

#m                   <- get_jmodel(virtual_workspace) # faster if you want to work in Java
#m                   <- get_r_model_from_j_model(m)  

from_reduced_to_full_JD_JSON_file(spec_file_name_csv)

from_full_to_reduced_JD_JSON_file(JD_JSON_file = JD_JSON_file_full_reduced)

compare_sa_ts(new_model_workspace = virtual_workspace_csv, old_model_workspace = input_workspace_directory , materialized_ws_new=FALSE, materialized_ws_old=TRUE, java_processing_old_model=FALSE)




