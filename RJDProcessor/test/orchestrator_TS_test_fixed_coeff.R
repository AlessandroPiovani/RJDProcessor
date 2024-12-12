setwd("C:\\Users\\UTENTE\\Desktop\\RJDopenCruncher\\RJDProcessor\\test\\")

library(RJDProcessor)
library(RJDemetra)


# JD_JSON_from_materialized_workspace <- function(workspace_directory, ext_reg_input_data_reader, regr_directory=NA, JSON_file_name = "JD_JSON_specification.txt", diff=TRUE, java_processing = TRUE)
# {
#   ws<-load_workspace(file = workspace_directory)
#   series_spec_list <- JD_JSON_from_virtual_workspace(ws, ext_reg_input_data_reader, JSON_file_name = JSON_file_name, diff=diff, java_processing = java_processing)
#   #return(series_spec_list)
# }
#
#
# JD_JSON_from_virtual_workspace <- function(ws, ext_reg_input_data_reader, JSON_file_name = "JD_JSON_specification.txt", diff=TRUE, java_processing=TRUE)
# {
#   #browser()
#
#   compute(ws)
#
#   series_spec_list  <-  extended_tramoseats_spec_list_from_workspace(workspace = ws, ext_reg_input_data_reader, java_processing=java_processing)
#
#   #browser()
#
#   # File opened in writing mode
#   con <- file(JSON_file_name, "w")
#
#   writeLines("[\n", con, sep = "")
#   n <- length(series_spec_list)
#
#   # Iterate over the specifications in the list, excluding the last, that will be printed after the for loop, without final comma
#   for (i in seq_len(n - 1)) {
#     # Obtain the current specification
#     #browser()
#     current_spec <- series_spec_list[[i]]
#
#
#     # Convert the specification in JSON format
#     json_spec <- to_JD_JSON(current_spec, indent = TRUE, diff=diff, basic_spec = current_spec@spec)
#
#     # Write the specification on the file
#     writeLines(json_spec, con, sep = ",\n\n\n")
#
#   }
#
#   # Last specification without "," at the end
#   current_spec <- series_spec_list[[n]]
#   json_spec <- to_JD_JSON(current_spec, indent = TRUE, diff = diff)
#   writeLines(json_spec, con, sep = "")
#
#   writeLines("\n]", con, sep = "")
#
#
#   writeLines("\n", con, sep = "") # to not have errors when you read again, files must end with "\n"
#
#   # Close the file
#   close(con)
#
#
#   #file.copy(from = "specifications_new.txt", to = "specifications_old.txt", overwrite = TRUE)
#
#
#   return(series_spec_list)
# }
#
#
# extended_tramoseats_spec_list_from_workspace <-  function(workspace, data_reader_ext_reg, method="TS", java_processing=TRUE ,...)
# {
#   compute(workspace)
#   #browser()
#
#   #jmodel          <- RJDemetra::get_jmodel(workspace, progress_bar = TRUE) # to retrieve external regressors by name
#   cat("Loading models\n")
#
#
#   browser()
#   # This part of code should be optimized (always do java)
#   if(java_processing == FALSE)
#   {
#     jm <- get_jmodel(workspace, progress_bar = FALSE) #added later: jmodel is necessary in any case for ramps and intervention variables
#     m <- get_model(workspace, progress_bar = TRUE)
#   }else
#   {
#     #browser()
#     m  <- get_jmodel(workspace, progress_bar = TRUE)
#     jm <- m
#     m  <- get_r_model_from_j_model(m)
#   }
#
#   cat("Loading external variables\n")
#   ##browser()
#   all_model_vars_info <- data_reader_ext_reg@read_ext_reg_info(workspace)
#
#   #all_jmodel_vars <- getUserDefinedTdVariables_info(jmodel) # per editare la scrittura
#
#   #browser()
#
#   extended_tramoseats_spec_list <- list()
#   for (series_name in names(m[[1]]))
#   {
#     series        <-  m[[1]][[series_name]]
#     frequency     <-  frequency(get_ts(series))
#
#     basic_spec    <- get_jspec(m[[1]][[series_name]])$toString()
#     #if(basic_spec=="TS") { basic_spec<-"RSA0" } #TS=custom spec --> by default set "RSA0" #encoded in the constructor call of Extended_tramoseats_spec
#
#     spec <- from_SA_spec(series, series_name = series_name, frequency = frequency, method = method, basic_spec=basic_spec, all_model_vars_info = all_model_vars_info, data_reader_ext_reg = data_reader_ext_reg)
#     spec <- list(spec)
#     extended_tramoseats_spec_list <- append(extended_tramoseats_spec_list ,spec)
#   }
#
#   gc() # to clean memory after the use of rJava
#
#   return(extended_tramoseats_spec_list)
#
# }


############################### Input defintion ################################
JD_JSON_file_full_reduced <- "specifications_new_full.txt"

spec_file_name            <- "specifications_new.txt"


input_workspace       <- "WorkspaceFATfixedCoeff-Container\\WS-FAT-fixed-coef.xml"
input_data_file_name  <- "SITIC-FAT\\grezzi.csv"
regr_directory        <- "SITIC-FAT\\regr"

# ripristinare lui
# input_workspace      <- "workspace_FAT_to_load\\workspace_FAT_to_load.xml"
# input_data_file_name <- "SITIC-FAT-10-2024\\grezzi.csv"
# regr_directory       <- "SITIC-FAT-10-2024\\regr"

# input_workspace      <- "WorkspaceFAT-container\\WS-FAT.xml"
# input_data_file_name <- "SITIC-FAT\\grezzi.csv"
# regr_directory       <- "SITIC-FAT\\regr"

# input_workspace      <- "WorkspaceTUR-container\\workspace-TUR.xml"
# input_data_file_name <- "SITIC-TUR\\grezziTUR.csv"
# regr_directory       <- "SITIC-TUR\\regr"

diff <- TRUE # Reduced JSON if diff=TRUE, Full JSON format otherwise

############################## Operational flow ################################




input_data_reader         <- Data_reader_csv_istat_format(input_source = input_data_file_name)
ext_reg_input_data_reader <- Data_reader_ext_reg_tsplus(regr_directory)


JD_JSON_from_materialized_workspace(input_workspace, ext_reg_input_data_reader, JSON_file_name = "specifications_new.txt", diff=TRUE, java_processing=FALSE)

series_to_proc_names <- NA #c("FATEXP_13", "C_DEFL", "FATEXP_14") # NA to process all the series #NA
virtual_workspace    <- JD_JSON_file_processor(input_data_reader = input_data_reader, ext_reg_data_reader = ext_reg_input_data_reader, spec_file_name = spec_file_name, output_workspace_dir = "output_workspace_container", series_to_proc_names = series_to_proc_names, java_processing = TRUE) # = NA) #output_workspace_dir can be omitted
# set java_processor=TRUE to speed-up the operations, but it does not work with workspaces readed by sa-ext plugin
m                    <- get_model(virtual_workspace) #get directly the R model (slower)

#m                   <- get_jmodel(virtual_workspace) # faster if you want to work in Java
#m                   <- get_r_model_from_j_model(m)

from_reduced_to_full_JD_JSON_file(spec_file_name)

from_full_to_reduced_JD_JSON_file(JD_JSON_file = JD_JSON_file_full_reduced)

compare_sa_ts(new_model_workspace = virtual_workspace, old_model_workspace = input_workspace , materialized_ws_new=FALSE, materialized_ws_old=TRUE, java_processing_old_model=FALSE)


















