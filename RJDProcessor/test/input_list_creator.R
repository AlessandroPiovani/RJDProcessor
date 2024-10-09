setwd("C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra_TEST_3\\")


#library(RJDemetra)
#library(rjson)

source("JD_JSON_file_processor.R")
source("JD_JSON.R")
source("Data_reader_list.R")
source("Data_reader_ext_reg_csv.R")

source("Data_reader_csv_istat_format.R")


input_data_file_name      <- "C:\\Users\\UTENTE\\Desktop\\MigrazioneFAT-RJDemetra_TEST_3\\SITIC-FAT\\grezzi.csv"
input_data_reader         <- Data_reader_csv_istat_format(input_source = input_data_file_name)
data<-input_data_reader@read_data()
row_names <- rownames(data)

FATEXP_10_data<-data[,"FATEXP_10"]
# Find the position of the first not NA value
first_non_na_pos <- which(!is.na(FATEXP_10_data))[1]
# Remove initial NAs
FATEXP_10_data_clean <- FATEXP_10_data[first_non_na_pos:length(FATEXP_10_data)]
FATEXP_10_data_clean <- gsub(",", ".", FATEXP_10_data_clean)
FATEXP_10_data_clean <- as.numeric(FATEXP_10_data_clean)
FATEXP_10_row_names_data_clean <- row_names[first_non_na_pos:length(FATEXP_10_data)]
FATEXP_10_list <- list("series_name"= "FATEXP_10", "dates"= FATEXP_10_row_names_data_clean, "values"= FATEXP_10_data_clean)
#FATEXP_10_list[["dates"]] <- as.Date(FATEXP_10_list[["dates"]])


C_DEFL_data<-data[,"C_DEFL"]
first_non_na_pos <- which(!is.na(C_DEFL_data))[1]
C_DEFL_data_clean <- C_DEFL_data[first_non_na_pos:length(C_DEFL_data)]
C_DEFL_data_clean <- gsub(",", ".", C_DEFL_data_clean)
C_DEFL_data_clean <- as.numeric(C_DEFL_data_clean)
C_DEFL_row_names_data_clean <- row_names[first_non_na_pos:length(C_DEFL_data)]
C_DEFL_list <- list("series_name"= "C_DEFL", "dates"= C_DEFL_row_names_data_clean, "values"= C_DEFL_data_clean)


FATEXP_14_data<-data[,"FATEXP_14"]
first_non_na_pos <- which(!is.na(FATEXP_14_data))[1]
FATEXP_14_data_clean <- FATEXP_10_data[first_non_na_pos:length(FATEXP_14_data)]
FATEXP_14_data_clean <- gsub(",", ".", FATEXP_14_data_clean)
FATEXP_14_data_clean <- as.numeric(FATEXP_14_data_clean)
FATEXP_14_row_names_data_clean <- row_names[first_non_na_pos:length(FATEXP_14_data)]
FATEXP_14_list <- list("series_name"= "FATEXP_14", "dates"= FATEXP_14_row_names_data_clean, "values"= FATEXP_14_data_clean)

input_list <- list(FATEXP_10_list, C_DEFL_list, FATEXP_14_list)

# per avere le date come stringhe (più facili da gestire)
input_list[[1]]$dates<- format(input_list[[1]]$dates, "%Y-%m-%d")
input_list[[2]]$dates<- format(input_list[[2]]$dates, "%Y-%m-%d")
input_list[[3]]$dates<- format(input_list[[3]]$dates, "%Y-%m-%d")

save(input_list, file = "input_list_FAT.RData")
load("input_list_FAT.RData")
