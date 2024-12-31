setwd("C:\\Users\\UTENTE\\Desktop\\RJDopenCruncher\\RJDProcessor\\test") #\\Workspace_from_scratch_container")

#install.packages("C:\\Users\\UTENTE\\Desktop\\RJDopenCruncher\\RJDProcessor_1.0.1.tar.gz", repos = NULL, type = "source")


library(RJDProcessor)
library(RJDemetra)



################################################################################
#                              Update R Workspace                              #
################################################################################
dr <- Data_reader_csv_istat_format(input_source = "SITIC-TUR//grezzi_TUR_updated.csv")
original_ws_xml <- "WorkspaceTUR-container/workspace-TUR.xml"
new_r_ws_folder <- "output_fully_R_ws"
regr_directory  <- "SITIC-TUR/regr"
ext_reg_input_data_reader <- Data_reader_ext_reg_tsplus(regr_directory)
produce_fully_R_workspace(ext_reg_input_data_reader, original_ws_xml, new_r_ws_folder)
dr <- Data_reader_csv_istat_format(input_source = "SITIC-TUR//grezzi_TUR_updated.csv")
update_data(workspace_xml_path = "output_fully_R_ws/workspace.xml", data_reader = dr)



################################################################################
#              Examples of working/non working conditions                      #
################################################################################

# Try to update a GUI created workspace --> ERROR
ws <- RJDemetra::load_workspace("C:\\Users\\UTENTE\\Desktop\\RJDopenCruncher\\RJDProcessor\\test\\Workspace_from_scratch_container\\Workspace_from_scratch.xml")
compute(ws)
update_data("C:\\Users\\UTENTE\\Desktop\\RJDopenCruncher\\RJDProcessor\\test\\Workspace_from_scratch_container\\Workspace_from_scratch.xml", data_reader = dr)

# Create a new workspace with RJDemetra function
ws <- RJDemetra::load_workspace("C:\\Users\\UTENTE\\Desktop\\RJDopenCruncher\\RJDProcessor\\test\\Workspace_from_scratch_container\\Workspace_from_scratch.xml")
compute(ws)
save_workspace(ws, "C:\\Users\\UTENTE\\Desktop\\RJDopenCruncher\\RJDProcessor\\test\\Workspace_from_scratch_container_R_from_GUI\\workspace.xml" )
# Update does not work if data are read from a provider (even if data are present in the XML file) --> ERROR
RJDProcessor::update_data("C:\\Users\\UTENTE\\Desktop\\RJDopenCruncher\\RJDProcessor\\test\\Workspace_from_scratch_container_R_from_GUI\\workspace.xml", data_reader = dr)


# Create and update a workspace with RJDProcessor --> It works
compute(ws)
dr         <- Data_reader_xlsx(input_source = "LCI.xlsx")
dr_ext_reg <- Data_reader_ext_reg_xlsx("C:\\Users\\UTENTE\\Desktop\\RJDopenCruncher\\RJDProcessor\\test\\Workspace_from_scratch_container\\regr") # Absolute path avoid problems
info <- dr_ext_reg@read_ext_reg_info(ws)
JD_JSON_from_materialized_workspace(ext_reg_input_data_reader = dr_ext_reg, JSON_file_name = "specs.txt", workspace = "Workspace_from_scratch_container_RJDProcessor\\Workspace_from_scratch.xml", regr_directory = "C:\\Users\\UTENTE\\Desktop\\RJDopenCruncher\\RJDProcessor\\test\\Workspace_from_scratch_container")
JD_JSON_to_materialized_workspace(input_data_reader = dr, ext_reg_data_reader = dr_ext_reg, JSON_file = "specs.txt", workspace_dir = "C:\\Users\\UTENTE\\Desktop\\RJDopenCruncher\\RJDProcessor\\test\\Workspace_from_scratch_container_RJDProcessor")
dr         <- Data_reader_xlsx(input_source = "LCI-updated.xlsx")
update_data(workspace_xml_path = "C:\\Users\\UTENTE\\Desktop\\RJDopenCruncher\\RJDProcessor\\test\\Workspace_from_scratch_container_RJDProcessor\\workspace.xml", data_reader = dr)


# Create and update a workspace with RJDProcessor, starting from an SA-plugin workspace --> it works

setwd("C:\\Users\\UTENTE\\Desktop\\RJDopenCruncher\\RJDProcessor\\test\\")


spec_file_name            <- "specs.txt"
input_workspace           <- "WorkspaceTUR-container\\workspace-TUR.xml"
input_data_file_name      <- "SITIC-TUR\\grezziTUR.csv"
regr_directory            <- "SITIC-TUR\\regr"

diff <- TRUE # Reduced JSON if diff=TRUE, Full JSON format otherwise


input_data_reader         <- Data_reader_csv_istat_format(input_source = input_data_file_name)
ext_reg_input_data_reader <- Data_reader_ext_reg_tsplus(regr_directory)


JD_JSON_from_materialized_workspace(input_workspace, ext_reg_input_data_reader, JSON_file_name = spec_file_name, diff=TRUE, java_processing=FALSE)
JD_JSON_to_materialized_workspace(input_data_reader = input_data_reader, ext_reg_data_reader = ext_reg_input_data_reader, JSON_file = spec_file_name, workspace_dir = "output_workspace_container")



# Update Original Workspace (from plugin) -> it doesn not works
dr <- Data_reader_csv_istat_format(input_source = "SITIC-TUR//grezzi_TUR_updated.csv")
update_data(workspace_xml_path = "WorkspaceTUR-container2//workspace-TUR.xml", data_reader = dr)


# Update RJDProcessor produced Workspace -> it works
dr <- Data_reader_csv_istat_format(input_source = "SITIC-TUR//grezzi_TUR_updated.csv")
update_data(workspace_xml_path = "output_workspace_container//workspace.xml", data_reader = dr)

