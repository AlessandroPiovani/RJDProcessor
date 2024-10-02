# Uncomment the rows to perform tests in debug mode with source code

# Basic import
rjdemetra_present     <- require(RJDemetra)
rjson_present         <- require(rjson)

if(!(rjdemetra_present && rjson_present))
{
  print("packages RJDemetra and rjson are required to use this package: make sure to have them installed on your computer")
}


# Interfaces for Data_reader and Data_reader_ext_reg
setGeneric("read_data", function(object, ...) standardGeneric("read_data"))
setGeneric("read_ext_reg_data", function(object, var_info=NULL, time_series_info=NULL, frequency= NA_integer_, ...) standardGeneric("read_ext_reg_data"))
setGeneric("read_ext_reg_info", function(object, var_info_container, adjust_path=TRUE, ...) standardGeneric("read_ext_reg_info"))

# source("Data_reader_xlsx.R")
# source("Data_reader_csv.R")
# source("Data_reader_list.R")
# source("Data_reader_xml.R")
# source("Data_reader_csv_istat_format.R")
# source("Data_reader_ext_reg_xlsx.R")
# source("Data_reader_ext_reg_tsplus.R")
# source("Data_reader_ext_reg_csv.R")
#
# source("utility_functions.R)
# source("basic_spec.R")
# source("Extended_tramoseats_spec.R")
# source("JD_JSON.R")
# source("JD_JSON_file_processor.R")
# source("workspaces_manager.R")


