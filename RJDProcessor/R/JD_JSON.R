
require(RJDemetra)
require(rjson)

#source("utility_functions.R")
#source("Extended_tramoseats_spec.R")





rJavaRampsAndIVsHandling <- function(sa, ramps, intervention_variables)
{
  
  Day      <- J("ec/tstoolkit/timeseries/Day")
  Ramp     <- J("ec/tstoolkit/timeseries/regression/Ramp")
  IV       <- J("ec/tstoolkit/timeseries/regression/InterventionVariable")
  Sequence <- J("ec/tstoolkit/timeseries/regression/Sequence")
  
  series_j <- sa$spec
  
  if(!(is.null(ramps) || all(is.na(ramps))))
  {
    for(ramp in ramps)
    {
      year_s  <- as.integer(substr(ramp$start, 1, 4))
      month_s <- as.integer(substr(ramp$start, 6, 7))
      day_s   <- as.integer(substr(ramp$start, 9, 10))
      #day_start <- Day$calc(year_s, month_s, day_s)
      day_start <- Day$calc(year_s, as.integer(month_s-1), as.integer(day_s-1))
      
      year_e  <- as.integer(substr(ramp$end, 1, 4))
      month_e <- as.integer(substr(ramp$end, 6, 7))
      day_e   <- as.integer(substr(ramp$end, 9, 10))
      day_end <- Day$calc(year_e, as.integer(month_e-1), as.integer(day_e-1))
      
      day_start_obj <- .jnew(Day, day_start)
      day_end_obj   <- .jnew(Day, day_end)
      
      r <-.jnew(Ramp, day_start_obj, day_end_obj)
      #browser()
      series_j$getCore()$getTramoSpecification()$getRegression()$add(r)
      
      jRegr<-series_j$getCore()$getTramoSpecification()$getRegression()
      
      #browser()
      if(ramp$fixed_coef!="NA" && !is.na(ramp$fixed_coef))
      {
        jRegr$setFixedCoefficients( r$getName(), .jarray(as.double(ramp$fixed_coef)))  
      }  
      
      
      
      #series_j$getCore()$getTramoSpecification()$getRegression()$setInterventionVariables(NULL)
    }
    #sa$spec <- series_j
    
  }
  #browser()
  if(!(is.null(intervention_variables) || all(is.na(intervention_variables))))
  {
    for(iv in intervention_variables)
    {
      #browser()
      seq_list <- list()
      
      idx_seq<-1
      for(sequ in iv$seq)
      {
        year_s  <- as.integer(substr(sequ$start, 1, 4))
        month_s <- as.integer(substr(sequ$start, 6, 7))
        day_s   <- as.integer(substr(sequ$start, 9, 10))
        day_start <- Day$calc(year_s, as.integer(month_s-1), as.integer(day_s-1))
        
        year_e  <- as.integer(substr(sequ$end, 1, 4))
        month_e <- as.integer(substr(sequ$end, 6, 7))
        day_e   <- as.integer(substr(sequ$end, 9, 10))
        day_end <- Day$calc(year_e, as.integer(month_e-1), as.integer(day_e-1))
        
        day_start_obj <- .jnew(Day, day_start)
        day_end_obj   <- .jnew(Day, day_end)
        s <-.jnew(Sequence, day_start_obj, day_end_obj)
        
        seq_list[[idx_seq]] <- s
        idx_seq <- idx_seq+1
      }
      # Convert Sequence list to Java array (Sequence[] in Java) and set it into the IV
      seq_array <- .jarray(seq_list, contents.class = "ec/tstoolkit/timeseries/regression/Sequence")
      
      intervention_variable <-.jnew(IV)
      
      
      intervention_variable$setSequences(seq_array)
      intervention_variable$setDelta(iv$delta)
      intervention_variable$setDelta(iv$delta_s)
      
      series_j$getCore()$getTramoSpecification()$getRegression()$add(intervention_variable)
    }
    
  }
  
  sa$spec <- series_j
  return(sa)
}





#' Turn a JD_JSON in a virtual workspace
#'
#' This function obtain a virtual workspace from a JD_JSON file.
#' See test foder for examples
#'
#' @param JSON_file Name of the JSON file to turn in a workspace (also with path).
#' @param input_data_reader A Data_Reader object
#' @param ext_reg_data_reader -optional- A Data_reader_ext_reg object, to read the external regressors
#'                            in the desired format (csv, xlsx, tramoseats+, ...)
#'                            Default = NA, do not consider external regressors (discouraged)
#' @param series_to_proc_names -optional- an array containing the name of the series to be included in the workspace
#'                            among the ones present in the JD_JSON file e.g. c("VATASA","VATAIA")
#' @return A virtual workspace
#' @examples
#' require(RJDemetra)
#' input_JD_JSON        <- system.file("extdata", "specifications_example1.txt", package = "RJDProcessor")
#' input_data_file_name <- system.file("extdata", "CSV-TUR/grezzi_trim_TUR.csv", package = "RJDProcessor")
#' regr_directory       <- system.file("extdata", "CSV-TUR/regr", package = "RJDProcessor")
#' input_data_reader         <- Data_reader_csv(input_source = input_data_file_name)
#' ext_reg_input_data_reader <- Data_reader_ext_reg_csv(regr_directory)
#' ws <- JD_JSON_to_virtual_workspace(input_JD_JSON, input_data_reader, ext_reg_input_data_reader, series_to_proc_names=NA)
#'
#' @export
JD_JSON_to_virtual_workspace <- function(JSON_file, input_data_reader, ext_reg_data_reader=NA, series_to_proc_names=NA)
{
  require(rJava)
  
  #browser()
  
  wk <- new_workspace()
  new_multiprocessing(wk, "sa1")
  
  #browser()
  
  
  mts_input_time_series <- input_data_reader@read_data() #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  
  timestamps   <- rownames(mts_input_time_series)
  series_names <- colnames(mts_input_time_series)
  for (i in 1:ncol(mts_input_time_series)) {
    
    #browser()
    if(all(is.na(series_to_proc_names)) || series_names[i] %in% series_to_proc_names)
    {
      #browser()
      # Extract the ts from mts matrix
      series <- mts_input_time_series[, i]
      
      # Find the position of the first non-NA value of the series
      start_index <- which(!is.na(series))[1]
      
      # Extract the part of the series which starts from the first not-NA value
      series <- gsub(",", ".", series)
      series_trimmed <- as.numeric(series[start_index:length(series)])
      
      # Compute the start based on the first not-NA value
      start_date <- timestamps[start_index]
      
      # auto detection of frequency
      d1         <- as.Date(timestamps[start_index])
      d2         <- as.Date(timestamps[start_index+1])
      month_diff <- abs(as.numeric(format(d1, "%m")) - as.numeric(format(d2, "%m")))
      freq       <- 12/month_diff
      
      
      # Extract year and month of start from the date
      year  <- as.integer(substr(start_date, 1, 4))  # Extract the first 4 characters (year) and cast them to integer
      month <- as.integer(substr(start_date, 6, 7))  # Extract characters 6 and 7 (month) and cast them to integer
      
      start <- c(year, month)
      
      #browser()
      
      
      # Build the ts object
      ts_obj  <- ts(series_trimmed, start = start, frequency = freq)
      
      ts_name <- series_names[i]
      
      #browser()
      extended_tramoseats_spec_list <- read_spec_list_from_json_file(JSON_file, spec_format="list")
      extended_tramoseats_spec_obj  <- extended_tramoseats_spec_list[[ts_name]]
      
      #browser()
      tramoseats_spec_args <- to_tramoseats_spec_args(extended_tramoseats_spec_obj, ext_reg_data_reader)
      
      # Handle Ramps and IVs that RJDemetra cannot receive in input, because it does not support them
      ramps                  <- tramoseats_spec_args$ramps
      intervention_variables <- tramoseats_spec_args$intervention_variables
      tramoseats_spec_args$ramps=NULL
      tramoseats_spec_args$intervention_variables=NULL
      #tramoseats_spec_args$ramps <-  empty arraylist()?? NOT HERE
      #tramoseats_spec_args$intervention_variables <- empty arraylist()?? NOT HERE
      
      #browser()
      easterCoef <- tramoseats_spec_args$easterCoef
      tramoseats_spec_args$easterCoef=NULL  # re-set it after do.call(...) 
      
      spec <- do.call(RJDemetra::tramoseats_spec, tramoseats_spec_args)
      #browser()
      sa <- jtramoseats(ts_obj, spec = spec)
      
      #browser()
      if(!is.null(easterCoef) && !is.na(easterCoef) && easterCoef!="NA")
      {
        jRegr<-sa$spec$getCore()$getTramoSpecification()$getRegression()
        jRegr$setFixedCoefficients( "easter", .jarray(as.double(easterCoef)))  
      }  
      sa <- rJavaRampsAndIVsHandling(sa, ramps, intervention_variables)
      
      #browser()
      
      add_sa_item(wk, "sa1", sa, ts_name)
    }
    
    
  }
  #browser()
  return(wk)
  
}


#' Turn a JD_JSON in a materialized workspace
#'
#' This function obtain a JD_JSON file from a workspace stored in the filesystem (in a directory).
#' See test foder for examples
#'
#' @param workspace_dir -optional- the directory of the input workspace.
#'                      Default = NA stores the workspace in a directory called "output_workspace_container"
#' @param JSON_file Name of the JSON file that will be produced (also with path).
#' @param input_data_reader A Data_Reader object
#' @param ext_reg_data_reader -optional- A Data_reader_ext_reg object, to produce the metadata of the external regressors
#'                            (i.e. create the names of the csv rather than a xlsx files o other containers for external regressors)
#'                            Default = NA, do not consider external regressors (discouraged)
#' @param series_to_proc_names -optional- an array containing the name of the series to be included in the workspace
#'                            among the ones present in the JD_JSON file e.g. c("VATASA","VATAIA")
#' @return void in R environment, a workspace materialized in the filesystem
#' @examples
#' require(RJDemetra)
#' input_JD_JSON        <- system.file("extdata", "specifications_example1.txt", package = "RJDProcessor")
#' input_data_file_name <- system.file("extdata", "CSV-TUR/grezzi_trim_TUR.csv", package = "RJDProcessor")
#' regr_directory       <- system.file("extdata", "CSV-TUR/regr", package = "RJDProcessor")
#' input_data_reader         <- Data_reader_csv(input_source = input_data_file_name)
#' ext_reg_input_data_reader <- Data_reader_ext_reg_csv(regr_directory)
#' JD_JSON_to_materialized_workspace(workspace_dir="ws_out" ,input_JD_JSON, input_data_reader, ext_reg_input_data_reader, series_to_proc_names=NA)
#'
#' @export
JD_JSON_to_materialized_workspace <- function(workspace_dir=NA, JSON_file, input_data_reader, ext_reg_data_reader=NA, series_to_proc_names=NA)
{
  require(rJava)
  
  #browser()
  
  wk <- JD_JSON_to_virtual_workspace(JSON_file, input_data_reader, ext_reg_data_reader, series_to_proc_names)
  
  if(is.na(workspace_dir))
  {
    workspace_dir <- "output_workspace_container"
  }
  
  #browser()
  # Extract the name of the directory
  dir_path      <- dirname(workspace_dir)
  workspace_dir <- basename(workspace_dir)
  
  # Check whether the path contains an absolute path or the directory name only
  if (dir_path == ".") {
    # If the path is only the directory name, set dir_path to getwd()
    dir_path <- getwd()
  }
  # else {
  #   # Else, extract the directory path (excluding the directory itself)
  #   dir_path <- dirname(workspace_dir)
  # }
  
  if (!file.exists(file.path(dir_path, workspace_dir))) {
    # If do not exist, create the dir_path directory
    dir.create(file.path(dir_path, workspace_dir))
  }
  dir_path <- file.path(dir_path, workspace_dir)
  
  
  compute(wk)
  #model=get_model(wk)
  
  workspace_file_path <- file.path(dir_path, "workspace.xml")
  save_workspace(wk, workspace_file_path)
  
  return(wk) #
}



#' Turn model spec of a materialized workspace in JD_JSON
#'
#' This function represent model specifications contained into a materialized workspace in JD_JSON
#'
#' @param workspace Name of the workspace xml file (also with path).
#' @param input_data_reader A Data_Reader object
#' @param ext_reg_input_data_reader A Data_reader_ext_reg object, to read the external regressors
#'                            in the desired format (csv, xlsx, tramoseats+, ...)
#' @param regr_directory -optional- Name of the directory containing the sources (e.g. files)
#'                            of the external regressors
#' @param JSON_file_name -optional- Name of the JSON file to be created. If NA the file will be
#'                       called "JD_JSON_specification.txt"
#' @param diff -optional- if TRUE a reduced version of the JSON specification is produced;
#'              In the reduced version, fields with default values equals to the ones of the
#'              default specification (i.e. "RSA0", "RSA1", ...) are not reported Default=TRUE
#' @param java_processing -optional- If TRUE, the function works internally with Java API (faster),
#'                        otherwise it uses R API. Default=FALSE
#' @return A JSON file saved on the filesystem
#' @examples
#'
#' require(RJDemetra)
#'
#' input_workspace <- system.file("extdata", "WorkspaceTUR-container/workspace-TUR.xml",
#'                                          package = "RJDProcessor")
#' input_data_file_name      <- system.file("extdata", "CSV-TUR/grezzi_trim_TUR.csv",
#'                                           package = "RJDProcessor")
#' regr_directory            <- system.file("extdata", "CSV-TUR/regr", package = "RJDProcessor")
#' diff <- TRUE   # Reduced JSON if diff=TRUE, Full JSON format otherwise
#'
#' input_data_reader         <- Data_reader_csv(input_source = input_data_file_name)
#' ext_reg_input_data_reader <- Data_reader_ext_reg_csv(regr_directory)
#' JD_JSON_from_materialized_workspace(input_workspace, ext_reg_input_data_reader, JSON_file_name = "specifications_new_ex2.txt", diff=TRUE, java_processing=FALSE)
#' @export
JD_JSON_from_materialized_workspace <- function(workspace, ext_reg_input_data_reader, regr_directory=NA, JSON_file_name = "JD_JSON_specification.txt", diff=TRUE, java_processing = FALSE)
{
  require(RJDemetra)
  require(rJava)
  
  ws<-load_workspace(file = workspace)
  series_spec_list <- JD_JSON_from_virtual_workspace(ws, ext_reg_input_data_reader, JSON_file_name = JSON_file_name, diff=diff, java_processing = java_processing)
  #return(series_spec_list)
}


#' Turn model spec of a virtual (R) workspace in JD_JSON
#'
#' This function represent model specifications contained into an R workspace in JD_JSON
#'
#' @param ws workspace R object.
#' @param input_data_reader A Data_Reader object
#' @param ext_reg_input_data_reader A Data_reader_ext_reg object, to read the external regressors
#'                            in the desired format (csv, xlsx, tramoseats+, ...)
#' @param regr_directory -optional- Name of the directory containing the sources (e.g. files)
#'                            of the external regressors
#' @param JSON_file_name -optional- Name of the JSON file to be created. If NA the file will be
#'                       called "JD_JSON_specification.txt"
#' @param diff -optional- if TRUE a reduced version of the JSON specification is produced;
#'              In the reduced version, fields with default values equals to the ones of the
#'              default specification (i.e. "RSA0", "RSA1", ...) are not reported Default=TRUE
#' @param java_processing -optional- If TRUE, the function works internally with Java API (faster),
#'                        otherwise it uses R API. Default=TRUE
#' @return A JSON file saved on the filesystem
#' @examples
#'
#' require(RJDemetra)
#'
#' input_workspace_directory <- system.file("extdata", "WorkspaceTUR-container/workspace-TUR.xml",
#'                                          package = "RJDProcessor")
#' input_data_file_name      <- system.file("extdata", "CSV-TUR/grezzi_trim_TUR.csv",
#'                                           package = "RJDProcessor")
#' regr_directory            <- system.file("extdata", "CSV-TUR/regr", package = "RJDProcessor")
#' diff <- TRUE   # Reduced JSON if diff=TRUE, Full JSON format otherwise
#'
#' input_data_reader         <- Data_reader_csv(input_source = input_data_file_name)
#' ext_reg_input_data_reader <- Data_reader_ext_reg_csv(regr_directory)
#'
#' original_directory <- getwd()
#' extdata_directory <- system.file("extdata", package = "RJDProcessor")
#' setwd(extdata_directory)
#'
#' ws <- load_workspace(file = input_workspace_directory)
#' JD_JSON_from_virtual_workspace(ws, ext_reg_input_data_reader, JSON_file_name = "specifications_new_out.txt", diff=TRUE, java_processing=FALSE)
#' setwd(original_directory)
#'
#' @export
JD_JSON_from_virtual_workspace <- function(ws, ext_reg_input_data_reader, JSON_file_name = "JD_JSON_specification.txt", diff=TRUE, java_processing=TRUE)
{
  require(RJDemetra)
  require(rJava)
  
  #browser()
  
  compute(ws)
  
  series_spec_list  <-  extended_tramoseats_spec_list_from_workspace(workspace = ws, ext_reg_input_data_reader, java_processing=java_processing)
  
  #browser()
  
  # File opened in writing mode
  con <- file(JSON_file_name, "w")
  
  writeLines("[\n", con, sep = "")
  n <- length(series_spec_list)
  
  # Iterate over the specifications in the list, excluding the last, that will be printed after the for loop, without final comma
  for (i in seq_len(n - 1)) {
    # Obtain the current specification
    #browser()
    current_spec <- series_spec_list[[i]]
    
    
    # Convert the specification in JSON format
    json_spec <- to_JD_JSON(current_spec, indent = TRUE, diff=diff, basic_spec = current_spec@spec)
    
    # Write the specification on the file
    writeLines(json_spec, con, sep = ",\n\n\n")
    
  }
  
  # Last specification without "," at the end
  current_spec <- series_spec_list[[n]]
  json_spec <- to_JD_JSON(current_spec, indent = TRUE, diff = diff)
  writeLines(json_spec, con, sep = "")
  
  writeLines("\n]", con, sep = "")
  
  
  writeLines("\n", con, sep = "") # to not have errors when you read again, files must end with "\n"
  
  # Close the file
  close(con)
  
  
  #file.copy(from = "specifications_new.txt", to = "specifications_old.txt", overwrite = TRUE)
  
  
  return(series_spec_list)
}


from_reduced_to_full_JD_JSON_obj<-function(JD_JSON_obj, basic_spec="RSA0")
{
  
  basic_spec   <- from_SA_spec(SA_spec = tramoseats_spec(basic_spec), userdef.varFromFile = FALSE)
  full_JD_JSON <- merge_objects_precedence_to_reduced(JD_JSON_obj, basic = basic_spec)
  return(full_JD_JSON)
}


merge_objects_precedence_to_reduced <- function(reduced, basic_spec = "RSA0")
{
  # Merge the objects with precedence over reduced
  full_object <- basic_spec
  names(full_object) <- c(names(reduced), setdiff(names(basic_spec), names(reduced)))
  for (key in names(reduced)) {
    full_object[[key]] <- reduced[[key]]
  }
  
  return(full_object)
}



#' Print a JSON file with all the JD_JSON fields explicit
#'
#' This function prints a JSON string that contains all the fields of the JD_JSON object explicitly defined.
#'
#' @param JD_JSON_file The name of the file in which the JD_JSON will be saved
#' @param output_file_name -optional- The name of the file (optionally with path) in
#'                         which the JD_JSON will be saved. If NA, the name of the
#'                         file will be paste0(JD_DSON_file,"_full")
#' @param indent -optional- Default TRUE. Print each field of the JSON in a
#'                          different row
#'
#' @return Void. A JSON file is saved on the filesystem with all the JD_JSON fields
#' @examples
#' original_directory <- getwd()
#' extdata_directory <- system.file("extdata", package = "RJDProcessor")
#' JSON_file_name <- system.file("extdata", "specifications_example2.txt", package = "RJDProcessor")
#' setwd(extdata_directory)
#' from_reduced_to_full_JD_JSON_file(JSON_file_name)
#' setwd(original_directory)
#' @export
from_reduced_to_full_JD_JSON_file <- function(JD_JSON_file, output_file_name=NA, indent= TRUE)
{
  require(RJDemetra)
  require(rJava)
  
  #browser()
  
  # Define file name
  if(is.na(output_file_name))
  {
    output_file_name <- gsub("\\.(\\w+)$", "_full.\\1", JD_JSON_file)
    
  }
  # Open the file in writing mode
  con <- file(output_file_name, "w")
  
  writeLines("[\n", con, sep = "")
  
  
  extended_tramoseats_spec_list <- read_spec_list_from_json_file(JD_JSON_file, spec_format="list")
  
  n <- length(extended_tramoseats_spec_list)
  
  for(i in seq_len(n - 1))
  {
    spec   <- extended_tramoseats_spec_list[[i]]
    
    #spec   <- convert_numerics_to_integers(spec) # Should I put it in the constructor
    #browser()
    spec   <- do.call(Extended_tramoseats_spec, spec)
    
    json_spec <- to_JD_JSON(spec, indent = indent, diff = FALSE)
    writeLines(json_spec, con, sep = ",\n\n\n")
  }
  
  #browser()
  spec   <- extended_tramoseats_spec_list[[n]]
  #spec   <- convert_numerics_to_integers(spec)
  spec   <- do.call(Extended_tramoseats_spec, spec)
  
  json_spec <- to_JD_JSON(spec, indent = indent, diff = FALSE)
  writeLines(json_spec, con, sep = "")
  
  writeLines("\n]", con, sep = "")
  
  
  writeLines("\n", con, sep = "") # to not have errors when you read again, files must end with "\n"
  
  # Close the file
  close(con)
  closeAllConnections()
  
}


#' Print a JSON file with only the fields that differ from the basic spec
#'
#' This function prints a JSON string that contains only the fields of the JD_JSON object that differ from the ones of the basic specification (i.e. "RSA0, "RSA1", ...)
#'
#' @param JD_JSON_file The name of the file in which the JD_JSON will be saved
#' @param output_file_name -optional- The name of the file (optionally with path) in
#'                         which the JD_JSON will be saved. If NA, the name of the
#'                         file will be paste0(JD_DSON_file,"_reduced")
#' @param indent -optional- Default TRUE. Print each field of the JSON in a
#'               different row
#' @param basic_spec -optional- i.e. "RSA0", "RSA1", ... . Default=NA. If NA,
#'                   use the basic spec specified in the input JSON
#' @return Void. A JSON file is saved on the filesystem
#' @examples
#' require(RJDemetra)
#' original_directory <- getwd()
#' extdata_directory <- system.file("extdata", package = "RJDProcessor")
#' JSON_file_name <- system.file("extdata", "specifications_example3_full.txt",
#'                                package = "RJDProcessor")
#' setwd(extdata_directory)
#' from_full_to_reduced_JD_JSON_file(JSON_file_name, "specification_new_out2.txt")
#' setwd(original_directory)
#' @export
from_full_to_reduced_JD_JSON_file<-function(JD_JSON_file, output_file_name=NA, indent= TRUE, basic_spec=NA)
{
  require(RJDemetra)
  require(rJava)
  
  #browser()
  
  # Define file name
  if(is.na(output_file_name))
  {
    output_file_name <- gsub("\\.(\\w+)$", "_reduced.\\1", JD_JSON_file)
    
  }
  # Open the file in writing mode
  con <- file(output_file_name, "w")
  
  writeLines("[\n", con, sep = "")
  
  #browser()
  extended_tramoseats_spec_list <- read_spec_list_from_json_file(JD_JSON_file, spec_format="list")
  
  n <- length(extended_tramoseats_spec_list)
  
  for(i in seq_len(n - 1))
  {
    #browser()
    
    spec   <- extended_tramoseats_spec_list[[i]]
    spec <- spec[!unlist(lapply(spec, function(x) all(is.na(x))))]
    
    #browser()
    if( is.na(basic_spec) )
    {
      basic_spec_f <- spec[["spec"]]
    } else
    {
      basic_spec_f <- basic_spec
    }
    
    
    #spec   <- convert_numerics_to_integers(spec) # Should I have to put it in the constructor?
    #browser()
    spec   <- do.call(Extended_tramoseats_spec, spec)
    
    json_spec <- to_JD_JSON(spec, indent = indent, diff = TRUE, basic_spec=basic_spec_f)
    writeLines(json_spec, con, sep = ",\n\n\n")
  }
  
  #browser()
  spec   <- extended_tramoseats_spec_list[[n]]
  spec   <- spec[!unlist(lapply(spec, function(x) all(is.na(x))))]
  
  if(is.na(basic_spec))
  {
    basic_spec_f <- spec[["spec"]]
  } else
  {
    basic_spec_f <- basic_spec
  }
  
  #spec   <- convert_numerics_to_integers(spec)
  spec   <- do.call(Extended_tramoseats_spec, spec)
  
  json_spec <- to_JD_JSON(spec, indent = indent, diff = TRUE, basic_spec=basic_spec_f)
  writeLines(json_spec, con, sep = "")
  
  writeLines("\n]", con, sep = "")
  
  
  writeLines("\n", con, sep = "") # to not have errors when you read again, files must end with "\n"
  
  # Close the file
  close(con)
  closeAllConnections()
  
  
}

