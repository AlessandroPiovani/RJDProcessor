check_character <- function(arg) {
  if (is.na(arg)|| arg=="NA") {
    return(NA_character_)
  } else {
    return(arg)
  }
  
}



span_unpack <- function(span_string) {
  output_list <- list()
  #browser()
  # Case 1: "All"
  if (tolower(span_string) == "all") {
    output_list$type <- "All"
    output_list$type.info <- NULL
    return(output_list)
  }
  
  # Case 2: "From YYYY-MM-DD"
  if (grepl("^from \\d{4}-\\d{2}-\\d{2}$", tolower(span_string))) {
    output_list$type <- "From"
    output_list$type.info <- substring(span_string, 6)
    return(output_list)
  }
  
  # Caso 3: "Until YYYY-MM-DD"
  if (grepl("^until \\d{4}-\\d{2}-\\d{2}$", tolower(span_string))) {
    output_list$type <- "To"
    output_list$type.info <- substring(span_string, 8)
    return(output_list)
  }
  
  # Case 4: "YYYY-MM-DD - YYYY-MM-DD"
  if (grepl("^\\d{4}-\\d{2}-\\d{2} - \\d{4}-\\d{2}-\\d{2}$", tolower(span_string))) {
    output_list$type <- "Between"
    output_list$type.info <- unlist(strsplit(span_string, " - "))
    return(output_list)
  }
  
  # Case 5: "last N periods"
  if (grepl("^all but last \\d+ periods$", tolower(span_string))) {
    output_list$type <- "Last"
    output_list$type.info <- as.integer(substring(span_string, 13, nchar(span_string)-8))
    return(output_list)
  }
  
  # Case 6: "first M periods"
  if (grepl("^all but first \\d+ periods$", tolower(span_string))) {
    output_list$type <- "First"
    output_list$type.info <- as.integer(substring(span_string, 14, nchar(span_string)-8))
    return(output_list)
  }
  
  # Case 7: "all but first N periods and last M periods"
  if (grepl("^all but first \\d+ periods and last \\d+ periods$", tolower(span_string))) {
    output_list$type <- "Exclude"
    parts <- strsplit(span_string, " ")[[1]]
    output_list$type.info <- c(as.integer(parts[4]), as.integer(parts[8]))
    return(output_list)
  }
  
  # No correspondance, return FALSE
  return(FALSE)
}

span_unpack_into_spec <- function(span = "All") {
  
  ret_list = list()
  ret_list$from      = NA_character_
  ret_list$to        = NA_character_
  ret_list$first     = NA_integer_
  ret_list$last      = NA_integer_
  ret_list$exclFirst = NA_integer_
  ret_list$exclLast  = NA_integer_
  
  # Utilize span_unpack function to obtain information about span
  span_info <- span_unpack(span)
  
  # Set father_list variables depending on span type
  if (span_info$type == "All") {
    ret_list$exclFirst <- as.integer(0)
    ret_list$exclLast <- as.integer(0)
  } else if (span_info$type == "From") {
    ret_list$from <- span_info$type.info
  } else if (span_info$type == "To") {
    ret_list$to <- span_info$type.info
  } else if (span_info$type == "Between") {
    ret_list$from <- span_info$type.info[1]
    ret_list$to <- span_info$type.info[2]
  } else if (span_info$type == "Last") {
    ret_list$last <- span_info$type.info
  } else if (span_info$type == "First") {
    ret_list$first <- span_info$type.info
  } else if (span_info$type == "Exclude") {
    ret_list$exclFirst <- span_info$type.info[1]
    ret_list$exclLast <- span_info$type.info[2]
  }
  
  return(ret_list)
}



get_outliers_info <- function(spec)
{
  
  #if(!is.null(spec$regression$userdef$outliers))
  if(spec$regression$userdef$specification$outlier==TRUE)
  {
    #browser()
    return(list(type=spec$regression$userdef$outliers$type, coeff=spec$regression$userdef$outliers$coeff, date=spec$regression$userdef$outliers$date))
  }
  else
  {
    return(list(type=NA, coeff=NA, date=NA))
  }
}

#ifelse(is.na(regarima_spec$arima$coefficients), NA, regarima_spec$arima$coefficients$Value)
get_arima_coef_info <- function(spec)
{
  # browser()
  if(spec$arima$specification$arima.coef == TRUE)
  {
    return(list(value=spec$arima$coefficients$Value, type=spec$arima$coefficients$Type))
  }
  else
  {
    return(list(value=NA, type=NA))
  }
}


get_user_def_var_info <- function(spec)
{
  #browser()
  if(spec$regression$userdef$specification$variables == TRUE)
  {
    # return(list(type=spec$regression$userdef$variables$description$type, coef=spec$regression$userdef$variables$description$coeff))
    
    coef_types  = spec$regression$userdef$variables$description$type
    coef_values = rep(NA, length(coef_types))
    
    if(spec$regression$userdef$specification$variables.coef==TRUE)
    {
      coef_values = spec$regression$userdef$variables$description$coeff
    }
    return(list(type=coef_types, coef=coef_values))
  }
  else
  {
    return(list(type=NA, coef=NA))
  }
}


difference_objects <- function(first, second) {
  #browser()
  
  # Empty list that will contain the elements which differ
  difference_list <- list()
  
  # Iterate over the elements of the first list
  for (name in slotNames(class(first))) {
    # if(name=="automdl.ub2"){
    #   browser()
    # }
    
    # Check whether the element exists in the second list and if values are different
    if (    (    name %in% slotNames(class(second))      )    && (     !identical(slot(first,name),slot(second,name))    )   ){
      #Add the element to the list of the differences
      difference_list[[name]] <- slot(first,name)
    }
    # If the element does not exist in the second list, add it to the second list anyway
    else if (!(name %in% slotNames(class(first)))) {
      difference_list[[name]] <- slot(first,name)
    }
  }
  
  return(difference_list)
}


# tramoseats_spec objects have 3 rows: 1) basic spec 2) spec build upon parameters passed from the user to the creation function 3) overlapping between 1 and 2
# this function keeps only the third (final) row, i.e. the one to be used in the further steps of the processing
simplify_leaves <- function(input_list) {
  
  
  result <- lapply(input_list, function(x) {
    
    #browser()
    
    if (is.data.frame(x)) {
      # If it is a dataframe with at least 3 rows, take only the first row
      if (nrow(x) == 3) {
        return(x[3, , drop = FALSE])
      }else if (nrow(x) >= 1) {  # Check if at least one row is present
        return(tail(x, n = 1))  # Return last row
      } else {
        return(NULL)  # Not enough rows to be mantained
      }
    } else if (is.list(x)) {
      # If it is a list, apply the function recursively
      return(simplify_leaves(x))
    } else if (is.vector(x)) {
      # If it is an array with at least 3 elements, take only the 3rd
      if (length(x) == 3) {
        return(x[3])
      } else {
        #return(NULL)  # There are not sufficient elements to be mantained
        return(x)
      }
    } else {
      # If it is neither a dataframe nor a list, return the object as it is aSe non è né un dataframe né una lista né un vettore, restituisci l'oggetto così com'è
      return(x)
    }
  })
  # browser()
  # Removes eventual null elements
  result <- result[!sapply(result, is.null)]
  
  return(result)
}

difference_objects_preserving_name_and_spec <- function(object, basic){
  #browser()
  series_name <- object@series_name # to be written in first position also if it is the same in the two objects
  frequency   <- object@frequency   # to be written also if it is the same in the two objects
  method      <- object@method      # to be written also if it is the same in the two objects
  spec        <- object@spec        # to be written also if it is the same in the two objects
  
  #original_object <- object
  object <- difference_objects(object, basic)
  
  object$series_name <- NULL
  object$frequency   <- NULL
  object$method      <- NULL
  object$spec        <- NULL
  
  first_positions <- list(series_name=series_name, frequency=frequency, method=method, spec=spec)
  object          <- c(first_positions, object)
  return(object)
}


NA_not_as_char <- function(json)
{
  remove_na <- function(x) {
    if (is.list(x)) {
      lapply(x, remove_na)
    } else {
      return(replace(x, x == "NA", NA))
    }
  }
  
  # Apply the function to every element of the list
  json <- lapply(json, remove_na)
  return(json)
}




convert_numerics_to_integers <- function(json_data, fields_to_convert=NA) {
  if(is.na(fields_to_convert))
  {
    fields_to_convert= c("estimate.first", "estimate.last", "estimate.exclFirst", "estimate.exclLast", "tradingdays.stocktd",
                         "easter.duration", "outlier.first", "outlier.last", "outlier.exclFirst", "outlier.exclLast",
                         "arima.p", "arima.d", "arima.q", "arima.bp", "arima.bd", "arima.bq")
  }
  
  for (field in fields_to_convert) {
    if (!is.null(json_data[[field]]) && is.numeric(json_data[[field]]) && all(json_data[[field]] %% 1 == 0)) {
      json_data[[field]] <- as.integer(json_data[[field]])
    }
  }
  return(json_data)
}

# get_userdef_var <- function(userdef.varFromFile, vars_mts, regarima_spec, series_name)
# {
#   if(userdef.varFromFile==TRUE){
#     return(vars_mts)
#   } else{
#    return(NA)
#   }
#
# }


extract_variable_names <- function(input_string) {
  # E.G. input_string: "Java-Object{{AO (2020-03-01)=[D@4ff3af97, AO (2020-06-01)=[D@5680f009, AO (2007-12-01)=[D@3a4e6da6, _ts_external_3@LYM_02_0=[D@73e0c775, _ts_external_2@TDU02M_0=[D@213d5189}}"
  # return "_ts_external_3@LYM_02_0" "_ts_external_2@TDU02M_0"
  # RegExp that finds the variableNames containing '@' and followed by '=['
  matches <- gregexpr("[^\\s,]+@[^\\s,]+(?=\\=\\[)", input_string, perl = TRUE)
  
  # Extract the results and put them into a list
  variables <- regmatches(input_string, matches)[[1]]
  
  # removes { if present at the beginning
  variables <- gsub("^\\{", "", variables)
  
  return(variables)
}

#' @export
get_mts <- function(time_series_matrix) {
  # Check if matrix has row and column names
  if (is.null(rownames(time_series_matrix)) || is.null(colnames(time_series_matrix))) {
    stop("Matrix must have names for rows and columns.")
  }
  
  # Estrai le date dalle righe
  date_index <- as.Date(rownames(time_series_matrix))
  
  # Check the difference in month between the first two dates, useful to obtain the frequency
  if (length(unique(date_index)) < 2) {
    stop("Matrix must contain at least two different dates")
  }
  
  date_diff <- as.numeric(difftime(unique(date_index)[2], unique(date_index)[1], units = "days")) / 30.44  # Circa 30.44 giorni per mese
  frequency <- round(12 / date_diff)  # Frequency
  
  # Create mts object
  mts_result <- ts(time_series_matrix,
                   start = c(as.numeric(format(min(date_index), "%Y")),
                             as.numeric(format(min(date_index), "%m"))),
                   frequency = frequency)
  
  return(mts_result)
}


#' Convert a numeric matrix to an mts object
#'
#' This function takes a numeric matrix with dates as row names and series names
#'  as column names, and converts it to an mts (multivariate time series) object.
#' It is goot for converting data read by data_readers and ext_reg_data_readers
#' into mts that could be useful as input as usrdef.var for RJDemetra functions
#' tramoseats() and X13()
#'
#' @param data_matrix A numeric matrix where row names are dates in "YYYY-MM-DD" format,
#'                    and column names are the names of the time series.
#' @param freq -optional- The frequency of the time series. If not provided, the function will
#'                        try to infer the frequency based on the difference between the first two dates.
#' @return An mts object with the given time series data.
#' @examples
#' input_data_file_name <- system.file("extdata","CSV-FAS/grezzi_trim_FAS.csv", package = "RJDProcessor")
#' input_data_reader    <- Data_reader_csv(input_source = input_data_file_name)
#' data_matrix <- input_data_reader@read_data()
#'
#' # Convert the matrix to an mts object
#' mts_object <- convert_numeric_matrix_to_mts(data_matrix)
#' print(mts_object)
#' @export
convert_numeric_matrix_to_mts <- function(data_matrix, freq = NULL) {
  # Convert row names to Date
  dates <- as.Date(rownames(data_matrix))
  
  # If frequency is not provided, try to infer it
  if (is.null(freq)) {
    d1 <- dates[1]
    d2 <- dates[2]
    month_diff <- abs(as.numeric(format(d1, "%m")) - as.numeric(format(d2, "%m")))
    freq <- 12 / month_diff
  }
  
  # Create a list of ts objects
  ts_list <- lapply(seq_len(ncol(data_matrix)), function(i) {
    ts(data_matrix[, i], start = c(as.numeric(format(dates[1], "%Y")), as.numeric(format(dates[1], "%m"))), frequency = freq)
  })
  
  # Combine ts objects into an mts object
  mts <- do.call(ts.union, ts_list)
  colnames(mts) <- colnames(data_matrix)
  
  return(mts)
}


reorder_ext_vars_td_at_the_end <- function(strings) {
  # The function separates the strings starting with "td|" and moves them to the end
  # while preserving their internal order.
  
  # Select the strings that start with "td|"
  td_strings <- strings[grep("^td\\|", strings)]
  
  # Select the strings that do not start with "td|"
  other_strings <- strings[!grepl("^td\\|", strings)]  # Use grepl for logical indexing
  
  # Combine the non-"td|" strings followed by the "td|" strings (internal order preserved)
  result <- c(other_strings, td_strings)
  
  return(result)
}
