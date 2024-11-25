setwd("C:\\Users\\UTENTE\\Desktop\\RJDopenCruncher\\RJDProcessor\\test")

library(RJDemetra)
library(RJDProcessor)
# TEST 1

single_workspaces_path <- "splitted_workspaces"
full_workspace         <- "WorkspaceTUR-container\\workspace-TUR.xml"
#full_workspace         <- "output_workspace_container_xlsx\\workspace.xml"
compressed_ws          <-  FALSE

get_single_ts_workspaces(full_workspace, single_workspaces_path, compressed_ws = compressed_ws )

single_ws<-load_workspace(paste0(single_workspaces_path,"\\VATPIC\\VATPIC.xml"))
check_data(c(1,2,3), single_ws)
check_external_regressors(single_ws)

#browser()

ws_merged           <- merge_workspaces(source_workspaces_path=single_workspaces_path, merged_ws_name = "merged_ws", compressed = FALSE, delete_originals = TRUE, silent=TRUE)
ws_merged_xml_path  <- paste0(single_workspaces_path,"\\","merged_ws","\\","merged_ws",".xml")


dr  <- RJDProcessor::Data_reader_csv_istat_format(input_source = "rawdata_ISTATformat_TUR.csv")
dr2 <- RJDProcessor::Data_reader_csv(input_source = "rawdata_TUR.csv")

#num_mat<-dr@read_data()

update_data(ws_merged_xml_path, dr2)

# TEST 2

single_workspaces_path <- "splitted_workspaces"
full_workspace         <- "WorkspaceFAT-container\\WS-FAT.xml"
compressed_ws          <-  TRUE

get_single_ts_workspaces(full_workspace, single_workspaces_path, compressed_ws = compressed_ws )


ws_merged           <- merge_workspaces(source_workspaces_path=single_workspaces_path, merged_ws_name = "merged_ws", compressed = TRUE, delete_originals = TRUE, silent=TRUE)
ws_merged_xml_path  <- paste0(single_workspaces_path,"\\","merged_ws","\\","merged_ws",".xml")


dr  <- RJDProcessor::Data_reader_csv_istat_format(input_source = "rawdata_ISTATformat_FAT.csv")

num_mat<-dr@read_data()

update_data(ws_merged_xml_path, dr)

get_single_ts_workspaces(full_workspace = ws_merged_xml_path, single_workspaces_path = single_workspaces_path, compressed_ws = TRUE )
#ws_merged <- merge_workspaces(source_workspaces_path=single_workspaces_path, merged_ws_name = "merged_ws", compressed = FALSE, delete_originals = TRUE, silent=TRUE)
#ws_merged <- merge_workspaces(source_workspaces_path=single_workspaces_path, merged_ws_name = "merged_ws2", merged_ws_dir = "C:\\Users\\UTENTE\\Desktop\\RJDopenCruncher\\RJDProcessor\\test\\merged_ws2" ,compressed = TRUE, delete_originals = FALSE, silent=TRUE)
