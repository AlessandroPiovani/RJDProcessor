setClass("Data_reader_ext_reg_tsplus",
         slots = list(
           input_source      = "ANY",   # in our case source is a directory path where the regressors are allocated
           read_ext_reg_data = "function",
           read_ext_reg_info = "function",
           ...               = "ANY"
         ))

#' Read external regressors data
#'
#' This function reads data from external regressors and returns it as a numeric matrix with variable names as colnames
#' and YYYY-MM-DD dates as rownames
#'
#' @param var_info A string with file name (also with path).
#' @param time_series_info A string with time series name in workspace name (also with path).
#' @param frequency i.e. 12 = monthly data, 4 = quarterly data
#' @return a numeric matrix with variable names as colnames and YYYY-MM-DD dates as rownames
#'
#' @examples
#'
#' require(RJDemetra)
#' input_workspace_xml       <- system.file("extdata", "WorkspaceTUR-container/workspace-TUR.xml",
#'                                          package = "RJDProcessor")
#' input_data_file_name      <- system.file("extdata", "SITIC-TUR/grezziTUR.csv", package = "RJDProcessor")
#' regr_directory            <- system.file("extdata", "SITIC-TUR/regr", package = "RJDProcessor")
#' ws                        <- load_workspace(file = input_workspace_xml)
#' compute(ws)
#' data_reader_ext_reg       <- Data_reader_ext_reg_tsplus(regr_directory)
#' all_model_ext_vars_info <- data_reader_ext_reg@read_ext_reg_info(ws)
#' vars_matrix             <- data_reader_ext_reg@read_ext_reg_data(all_model_ext_vars_info, "VATASC",
#'                                                                  frequency=12)
#'
#' @export
setMethod ("read_ext_reg_data", signature("Data_reader_ext_reg_tsplus", "ANY", "ANY", "ANY"),
           function(object, var_info=NULL, time_series_info=NULL, frequency=NA_integer_, ...) {
             
             
             series_name <- time_series_info
             
             # if(series_name=="C_DEFL")
             # {
             #   browser()
             # }
             
             
             reg_directory   <- object@input_source
             
             if(is.null(var_info) || length(var_info)==0)
             {
               return(NA)
             }
             
             ts_list <-list()
             
             
             mts_total <- NA
             # Iterate through var_info_list elements
             for (i in seq_along(var_info[[series_name]]))
             {
               
               ##browser()
               user_def_var <- var_info[[series_name]][[i]]
               
               # Build the complete file path
               full_file_path <- file.path(reg_directory, user_def_var$container)
               
               # Read data from file
               data <- read.table(full_file_path, header = FALSE, sep = "\t")
               
               # Set start date and frequency
               data_date <- as.Date(user_def_var$start)
               y <- as.integer(format(data_date, "%Y"))
               m <- as.integer(format(data_date, "%m"))
               start_date <- c(y, m)
               
               
               # frequency  <- user_def_var$frequency # if I passed the frequency as a metadata
               if(is.na(frequency)) # Auto detect of frequency, not possible for TS txt format (this if is never used)
               {
                 #Auto detect of frequency from the data
                 #browser()
                 timestamps  <- rownames(data)
                 start_index <- which(!is.na(series))[1]
                 d1          <- as.Date(timestamps[start_index])
                 d2          <- as.Date(timestamps[start_index+1])
                 month_diff  <- abs(as.numeric(format(d1, "%m")) - as.numeric(format(d2, "%m")))
                 frequency   <- 12/month_diff
               }
               
               mts_file<-NA
               # Iterates over file's columns
               for (j in 1:ncol(data))
               {
                 # Get column name
                 column_name <- user_def_var$container
                 column_name <- gsub("\\.txt", "", column_name)
                 
                 # Get column data
                 column_data <- data[[j]]
                 
                 # create a dummy variable allowing to work with an MTS instead of a TS. Working with MTS allows to set the series names
                 if(j==1 && length(ts_list)==0)
                 {
                   dummy <- ts(data = column_data, class="ts", frequency = frequency, start = start_date)
                   ts_list <- list("dummy"=dummy)
                   mts_file_col_names <- c("dummy")
                 }
                 
                 # Case of the 6 Trading Days variables in one file
                 if(user_def_var$n_var > 1)
                 {
                   column_name <- paste(column_name, "_", j, sep="")
                 }
                 mts_file_col_names <- c(mts_file_col_names, column_name)
                 
                 time_series_new <- ts(column_data, start = start_date, frequency = frequency)
                 
                 ts_list[[column_name]] <- time_series_new
               }
             }
             
             if(length(ts_list)>0)
             {
               mts_file <- do.call(ts.union, ts_list)
               
               colnames(mts_file) <- mts_file_col_names
               mts_file <- mts_file[,-1]
               if(all(is.na(mts_total)))
               {
                 mts_total <- mts_file#NA
               }else
               {
                 mts_total <- cbind(mts_total, mts_file)
               }
               mts_file<-NA
             }
             
             #browser()
             if(length(class(mts_total)=="ts")==1 && class(mts_total)=="ts")
             {
               #browser()
               data_ts <- as.vector(mts_total)
               dim(data_ts) <- c(length(mts_total), 1)
               dimnames(data_ts) <- list(NULL, column_name)
               mts_total <- ts(data = data_ts, start = start_date, frequency = frequency, class=c("mts", "ts", "matrix"), names=c(column_name))
             }
             
             
             return(mts_total)
             
             
           })

#' Read information about external regressors from a workspace
#'
#' This function returns a list of information about external regressors used in the models contained in a workspaces
#'
#' @param var_info_container workspace xml file path
#' @return list() of information about external regressors
#' @examples
#'
#' require(RJDemetra)
#' input_workspace_xml       <- system.file("extdata", "WorkspaceTUR-container/workspace-TUR.xml",
#'                                          package = "RJDProcessor")
#' input_data_file_name      <- system.file("extdata", "SITIC-TUR/grezziTUR.csv", package = "RJDProcessor")
#' regr_directory            <- system.file("extdata", "SITIC-TUR/regr", package = "RJDProcessor")
#' ws                        <- load_workspace(file = input_workspace_xml)
#' compute(ws)
#' data_reader_ext_reg       <- Data_reader_ext_reg_tsplus(regr_directory)
#' all_model_ext_vars_info <- data_reader_ext_reg@read_ext_reg_info(ws)
#'
#' @export
setMethod ("read_ext_reg_info", signature("Data_reader_ext_reg_tsplus", "ANY", "ANY"),
           function(object, var_info_container, adjust_path=TRUE, ...) {
             
             workspace <- var_info_container
             
             jmodel <- RJDemetra::get_jmodel(workspace, progress_bar = TRUE) # to retrieve external regressors by name
             
             all_jmodel_vars <- getUserDefinedTdVariables_info_tsplus(jmodel, adjust_path=adjust_path) # per editare la scrittura
             return(all_jmodel_vars)
             
           })


#' Constructor (R-like) of the Data_reader object
#'
#' This function creates a Data_reader_ext_reg object capable of reading data from TRAMO-SEATS+ external regressors files and returning it using the \code{read_ext_reg_data()} function.
#'
#' @param input_source A string with the input: e.g. a file name (also with path) if the input is a file.
#' @return The Data_reader_ext_reg_tsplus object
#' @examples
#' require(RJDemetra)
#' input_workspace_xml       <- system.file("extdata", "WorkspaceTUR-container/workspace-TUR.xml",
#'                                          package = "RJDProcessor")
#' input_data_file_name      <- system.file("extdata", "SITIC-TUR/grezziTUR.csv", package = "RJDProcessor")
#' regr_directory            <- system.file("extdata", "SITIC-TUR/regr", package = "RJDProcessor")
#' ws                        <- load_workspace(file = input_workspace_xml)
#' compute(ws)
#' data_reader_ext_reg       <- Data_reader_ext_reg_tsplus(regr_directory)
#' all_model_ext_vars_info <- data_reader_ext_reg@read_ext_reg_info(ws)
#' vars_matrix             <- data_reader_ext_reg@read_ext_reg_data(all_model_ext_vars_info, "VATASC",
#'                                                                  frequency=12)
#'
#' @export
Data_reader_ext_reg_tsplus <- function(input_source, ...) {
  
  obj <- new("Data_reader_ext_reg_tsplus", input_source = input_source, read_ext_reg_data = function(...) read_ext_reg_data(obj, ...), read_ext_reg_info = function(...) read_ext_reg_info(obj,...),...)
  return(obj)
}

















# The variables values are available in files.
# input_mode = TS_regressor_file the start date is the same as the input data series
# input_mode = TODO JD_regressor_file file containing both dates and data
getUserDefinedTdVariables_info_tsplus <- function(jmodel ,input_mode=c("TS_regressor_file", "JD_regressor_file", ), adjust_path=TRUE)
{
  var_info_list   = list()
  ramps_info_list = list()
  iv_info_list    = list()
  coef_info_list  = list()
  #browser()
  
  get_repeat_counts <- function(jUser_Td_VarsString) {
    # Check whether jUser_Td_VarsString is a single string
    if (is.character(jUser_Td_VarsString) && length(jUser_Td_VarsString) == 1) {
      return(1)  # Returns an array with only one element having value 1
    } else {
      # Extract prefix from each element
      prefixes <- gsub("^(.*)_\\d+$", "\\1", jUser_Td_VarsString)
      # Count the repetitions of the prefix
      repeat_counts <- table(prefixes)
      # Creation of the array of the results
      result <- integer(length(prefixes))
      # Assign the number of repetitions of each variable to the vector of the results
      for (i in seq_along(prefixes)) {
        result[i] <- repeat_counts[prefixes[i]]
      }
      return(result)  # Return an array with the number of repeted variables
    }
  }
  repeated_vars <- c()
  
  require(rJava)
  for(name in names(jmodel[[1]]))
  {
    # if(name=="C_DEFL")
    # {
    #  browser()
    # }
    
    jSA_series      <- jmodel[[1]][[name]]
    jRegression     <- jSA_series$spec$getRegression()
    jCalendar       <- jRegression$getCalendar()
    jTradingDays    <- jCalendar$getTradingDays()
    jUser_Td_VarsString      <- jTradingDays$getUserVariables()
    file_list <- list()
    
    coef_list <- list() #Added
    
    idx_file_list = 1
    
    usrDefVarCount    <- jRegression$getUserDefinedVariablesCount()
    
    if(usrDefVarCount > 0)
    {
      var_info_list[[name]] = list()
      
      file_list = list()
      previous=list()
      for(i in 1:usrDefVarCount)
      {
        #usr def loop
        ##browser()
        j <- i-1             #Added this part
        j <- as.integer(j)
        jUser_UserDef_Var <- jRegression$getUserDefinedVariable(j) # Returns all 0, don't know why
        #coef_list[[i]] <- jUser_UserDef_Var$getCoefficient() # Do the same for TD, checking for the index. Return coef_list
        
        #browser()
        
        
        varString         <- jUser_UserDef_Var$getName()
        
        # BLOCK Designed over specific regressors presents in ISTAT
        if (grepl("lym_", varString, ignore.case = TRUE))
        {
          #browser()
          file_name <- tolower(sub("^[^.]*\\.", "", varString))
          file_name <- sub("^(.*lym_[0-9]+).*", "\\1", file_name)
        } else if (grepl("q3_00_04", varString))
        {
          file_name <- "q3_00_04"
        } else if (grepl("^[^.]*\\.q[0-9]+_[0-9]+", varString, ignore.case = TRUE))
        {
          file_name <- tolower(sub("^[^.]*\\.", "", varString))
          file_name <- sub("^(.*q_[0-9]+).*", "\\1", file_name)
        } else # the only one action to do if no exceptions are considered!!!!!
        {
          file_name <- tolower(sub("^[^.]*\\.", "", varString))
          #file_name <- tolower(sub(".*\\.(.*?)_\\d+$", "\\1", file_name))
          file_name <- tolower(sub("(.*?)_\\d+$", "\\1", file_name))
        }
        
        #file_name <- gsub("^[rR]\\.", "", file_name)
        file_name  <- paste0(file_name, ".txt")
        
        
        #Removes part of path e.g. regr\\file.txt or regr/file.txt  --> file.txt
        if(adjust_path==TRUE)
        {
          file_name <- sub(".*[\\/]", "", file_name)
        }
        
        # END BLOCK
        
        year       <- start(get_ts(jSA_series))[1]
        month      <- start(get_ts(jSA_series))[2]
        start_date <- as.Date(paste(year, month, "01", sep = "-"))
        
        start_date <- format(start_date, "%Y-%m-%d")
        current    <- list(container = file_name, start = start_date, n_var=1) #frequency=frequency(get_ts(jSA_series)), #, type="userdef")
        if (!identical(current, previous)) # for the 6 variables case: all the variables belong to the same file, so the fileinfo will be replicated 6 time, but i save only one of them not incrementing idx_file_list
        {
          file_list[[idx_file_list]] <- current
          previous = file_list[[idx_file_list]]
          idx_file_list=idx_file_list+1
          
        }
        
        
        
      }
      #var_info_list[[name]] <- append(var_info_list[[name]], file_list)
      
    }
    
    idx_coef <-usrDefVarCount+1
    
    if(length(jUser_Td_VarsString)>0)
    {
      # if (!(name %in% names(var_info_list)))
      # {
      #   var_info_list[[name]] = list()
      # }
      
      #browser()
      #i = length(var_info_list[[name]])+1 # if some usrDefVar have been already added, I start adding the TD user defined vars after them
      i       = idx_file_list
      repeated_vars <- get_repeat_counts(jUser_Td_VarsString)
      r_idx = 1 # repeated_vars index
      
      previous=list()
      for(varString in jUser_Td_VarsString)
      {
        # No fixed coefficients for TD. If You want them to be fixed, put into UsrDefinedVariables
        #browser()
        #coef_list[[idx_coef]] <- 1#varStringEstrarreCoeffSecE
        #idx_coef <- idx_coef+1
        
        
        # BLOCK Designed over specific regressors presents in ISTAT
        if (grepl("lym_", varString, ignore.case = TRUE))
        {
          #browser()
          file_name <- tolower(sub("^[^.]*\\.", "", varString))
          file_name <- sub("^(.*lym_[0-9]+).*", "\\1", file_name)
        } else if (grepl("q3_00_04", varString))
        {
          file_name <- "q3_00_04"
        } else if (grepl("^[^.]*\\.q[0-9]+_[0-9]+", varString, ignore.case = TRUE))
        {
          file_name <- tolower(sub("^[^.]*\\.", "", varString))
          file_name <- sub("^(.*q_[0-9]+).*", "\\1", file_name)
        } else # the only one action to do if no exceptions are considered!!!!!
        {
          file_name <- tolower(sub("^[^.]*\\.", "", varString))
          #file_name <- tolower(sub(".*\\.(.*?)_\\d+$", "\\1", file_name))
          file_name <- tolower(sub("(.*?)_\\d+$", "\\1", file_name))
        }
        
        #file_name <- gsub("^[rR]\\.", "", file_name)
        file_name  <- paste0(file_name, ".txt")
        
        
        #Removes part of path e.g. regr\\file.txt or regr/file.txt  --> file.txt
        if(adjust_path==TRUE)
        {
          file_name <- sub(".*[\\/]", "", file_name)
        }
        # END BLOCK
        
        
        
        
        year  <- start(get_ts(jSA_series))[1]
        month <- start(get_ts(jSA_series))[2]
        start_date <- as.Date(paste(year, month, "01", sep = "-"))
        
        #browser()
        
        
        start_date <- format(start_date, "%Y-%m-%d")
        current = list(container = file_name, start = start_date, n_var=repeated_vars[r_idx]) ##frequency=frequency(get_ts(jSA_series)), # type="td")
        r_idx <- r_idx + 1
        if (!identical(current, previous))  # for the 6 variables case: all the variables belong to the same file, so the fileinfo will be replicated 6 time, but i save only one of them not incrementing i
        {
          file_list[[i]] = current
          previous       = file_list[[i]]
          i=i+1
        }
      }
    }
    
    #### Copy from here and the beginning if the function
    # if(name=="C_DEFL")
    # {
    #   browser()
    # }
    var_info_list[[name]] <- append(var_info_list[[name]], file_list)
    
    ramps_list   <- get_ramps(jSA_series)
    ramps_info_list[[name]] <- append(ramps_info_list[[name]], ramps_list)
    
    ivs_list     <- get_intervention_vars(jSA_series)
    iv_info_list[[name]] <- ivs_list
    
    #coef_info_list[[name]] = append(coef_info_list[[name]], coef_list)
    
    # Fixed coefficients reading
    #browser()
    jtramoSeatsSpec<-jmodel[[1]][[name]]
    res<-jtramoSeatsSpec$result
    jCore<-jtramoSeatsSpec$spec$getCore()
    tramoSpec<-jCore$getTramoSpecification()
    jReg<-tramoSpec$getRegression()
    allFixedRegr <- jReg$getAllFixedCoefficients()
    allFixedRegrS<- .jstrVal(allFixedRegr)
    fixedUsrDefVarNames <- extract_variable_names(allFixedRegrS) #extract_variable_names() code is in utility_functions.R
    
    ############################### start added ################################
    TsFrequency <- J("ec/tstoolkit/timeseries/simplets/TsFrequency")
    
    jsa  <- jmodel[[1]][name][[1]]
    ts   <- get_indicators(jsa, "y")
    freq <- 12/frequency(ts)
    if(freq==12)
    {
      freq_enum <- TsFrequency$Monthly
    } else { freq_enum <- TsFrequency$Quarterly } 

    varNames           <- jReg$getRegressionVariableNames(freq_enum)
    #browser()
    filtered_var_names <- varNames[grepl("@", varNames)]
    filtered_var_names <- reorder_ext_vars_td_at_the_end(filtered_var_names)
   
    # change with respect to the original
    
    #coef_list <- c()
    for(var_name in filtered_var_names)
    {
      #idx_coef_list=1
      if(var_name %in% fixedUsrDefVarNames)
      {
        coef<-jReg$getFixedCoefficients(var_name)
        coef_list <- append(coef_list, coef)
      } else
      {
        #coef_list <- append(coef_list, NA) # put 0 instead of NA?
        coef_list <- append(coef_list, 0) # error in some part of my code if i put NA
      }  
    }  
    coef_info_list[[name]] = append(coef_info_list[[name]], coef_list)
    
    ############################# end added/modified ###########################
    
    #browser()
    
  }
  
  ret <- list("ext_vars"=var_info_list, "ramps"=ramps_info_list, "intervention_vars"=iv_info_list, "varCoef"=coef_info_list)
  return(ret)
}

# possible conflicts! leave these functions (get_ramps, get_intervention_vars) equals in all the scripts
# TODO: move in utility_functions or assign a different name in every Data_reader_ext_reg!
get_ramps<- function(series)
{
  ramps_ret <- list()
  
  regression<-series$spec$getRegression()
  core<-regression$getCore()
  core_regression<-core$getRegression()
  ramps_count<-core_regression$getRampsCount() # for loop over this
  
  i<-0
  while(i < ramps_count)
  {
    ramp_i       <-core_regression$getRamp(as.integer(i))
    begin_ramp_i <- ramp_i$getStart()$toString()
    end_ramp_i   <- ramp_i$getEnd()$toString()
    ramp_i_ret   <- list("start"=begin_ramp_i, "end"=end_ramp_i)
    
    ramps_ret[[length(ramps_ret) + 1]] <- ramp_i_ret
    
    i<-i+1
  }
  return(ramps_ret)
  
  
}

get_intervention_vars<- function(series)
{
  #browser()
  ivs_ret <- list()
  
  regression<-series$spec$getRegression()
  core<-regression$getCore()
  core_regression<-core$getRegression() #until here in common with RAMPS
  intervention_vars_count <- core_regression$getInterventionVariablesCount()
  i<-0
  
  while(i<intervention_vars_count)
  {
    int_var_i<-core_regression$getInterventionVariable(as.integer(i))
    int_var_i$getName()
    iv_i_delta     <- int_var_i$getDelta()
    iv_i_deltaS    <- int_var_i$getDeltaS()
    iv_i_sequences <- int_var_i$getSequences() #every IV could be created in different times thanks to multiple sequences associated
    n_seq <- iv_i_sequences$length #iterate over this
    j<-0
    seq_list <- list()
    while(j < n_seq)
    {
      iv_i_sequences_seq_j       <- int_var_i$getSequence(as.integer(j))
      iv_i_sequences_seq_j_start <- iv_i_sequences_seq_j$start$toString()
      iv_1_sequences_seq_j_end   <- iv_i_sequences_seq_j$end$toString()
      
      iv_1_sequences_seq_j_to_save <- list("start"=iv_i_sequences_seq_j_start, "end"= iv_1_sequences_seq_j_end)
      
      seq_list[[length(seq_list) + 1]] <- iv_1_sequences_seq_j_to_save
      j<-j+1
    }
    
    iv<- list("delta"=iv_i_delta, "delta_s"=iv_i_deltaS , "seq"=seq_list)
    ivs_ret[[length(ivs_ret) + 1]] <- iv
    
    i <- i+1
  }
  
  return(ivs_ret)
  
}
