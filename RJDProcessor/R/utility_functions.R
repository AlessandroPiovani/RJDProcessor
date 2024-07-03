check_character <- function(arg) {
  if (is.na(arg)|| arg=="NA") {
    return(NA_character_)
  } else {
    return(arg)
  }
  
}


span_unpack <- function(span_string) {
  # Inizializza la lista di output
  output_list <- list()
  #browser()
  # Caso 1: "All"
  if (tolower(span_string) == "all") {
    output_list$type <- "All"
    output_list$type.info <- NULL
    return(output_list)
  }
  
  # Caso 2: "From YYYY-MM-DD"
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
  
  # Caso 4: "YYYY-MM-DD - YYYY-MM-DD"
  if (grepl("^\\d{4}-\\d{2}-\\d{2} - \\d{4}-\\d{2}-\\d{2}$", tolower(span_string))) {
    output_list$type <- "Between"
    output_list$type.info <- unlist(strsplit(span_string, " - "))
    return(output_list)
  }
  
  # Caso 5: "last N periods"
  if (grepl("^all but last \\d+ periods$", tolower(span_string))) {
    output_list$type <- "Last"
    output_list$type.info <- as.integer(substring(span_string, 13, nchar(span_string)-8))
    return(output_list)
  }
  
  # Caso 6: "first M periods"
  if (grepl("^all but first \\d+ periods$", tolower(span_string))) {
    output_list$type <- "First"
    output_list$type.info <- as.integer(substring(span_string, 14, nchar(span_string)-8))
    return(output_list)
  }
  
  # Caso 7: "all but first N periods and last M periods"
  if (grepl("^all but first \\d+ periods and last \\d+ periods$", tolower(span_string))) {
    output_list$type <- "Exclude"
    parts <- strsplit(span_string, " ")[[1]]
    output_list$type.info <- c(as.integer(parts[4]), as.integer(parts[8]))
    return(output_list)
  }
  
  # Nessuna corrispondenza, restituisci FALSE
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
  
  # Utilizza la funzione span_unpack per ottenere le informazioni sullo span
  span_info <- span_unpack(span)
  
  # Imposta le variabili in father_list in base al tipo di span
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

  # Inizializza una lista vuota per contenere gli elementi differenziali
  difference_list <- list()
  
  # Itera attraverso gli elementi della prima lista
  for (name in slotNames(class(first))) {
    # if(name=="automdl.ub2"){
    #   browser()
    # }
    
    # Controlla se l'elemento esiste nella seconda lista e se i valori sono diversi
    if (    (    name %in% slotNames(class(second))      )    && (     !identical(slot(first,name),slot(second,name))    )   ){
      # Aggiungi l'elemento alla lista delle differenze
      difference_list[[name]] <- slot(first,name)
    }
    # Se l'elemento non esiste nella seconda lista, aggiungilo comunque alla lista delle differenze
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
      # Se è un dataframe con almeno 3 righe, prendi solo la prima riga
      if (nrow(x) == 3) {
        return(x[3, , drop = FALSE])
      }else if (nrow(x) >= 1) {  # Controlla se ci sono almeno una riga
        return(tail(x, n = 1))  # Ritorna l'ultima riga
      } else {
        return(NULL)  # Non ci sono sufficienti righe da mantenere
      } 
    } else if (is.list(x)) {
      # Se è una lista, applica ricorsivamente la funzione
      return(simplify_leaves(x))
    } else if (is.vector(x)) {
      # Se è un vettore con almeno 3 elementi, prendi solo il terzo
      if (length(x) == 3) {
        return(x[3])
      } else {
        #return(NULL)  # Non ci sono sufficienti elementi da mantenere
        return(x)
      }
    } else {
      # Se non è né un dataframe né una lista né un vettore, restituisci l'oggetto così com'è
      return(x)
    }
  })
  # browser()
  # Rimuovi eventuali elementi NULL
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
  
  # Applica la funzione a ciascun elemento della lista
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


