# source("basic_spec.R")
require(RJDemetra)


# Class declaration
setClass(
  "Extended_tramoseats_spec",
  slots = list(
    series_name             = "character",    # ISTAT custom field
    frequency               = "integer",
    method                  = "character",
    spec                    = "character",
    preliminary.check       = "logical",
    estimate.from           = "character",
    estimate.to             = "character",
    estimate.first          = "integer",
    estimate.last           = "integer",
    estimate.exclFirst      = "integer",
    estimate.exclLast       = "integer",
    estimate.tol            = "numeric",
    estimate.eml            = "logical",
    estimate.urfinal        = "numeric",
    transform.function      = "ANY",
    transform.fct           = "numeric",
    usrdef.outliersEnabled  = "logical",
    usrdef.outliersType     = "ANY",
    usrdef.outliersDate     = "ANY",
    usrdef.outliersCoef     = "ANY",
    userdef.varFromFile     = "logical",        # ISTAT custom field
    userdef.varFromFile.infoList  = "ANY",      # ISTAT custom field
    usrdef.varEnabled       = "logical",
    usrdef.var              = "ANY",
    usrdef.varType          = "ANY",
    usrdef.varCoef          = "ANY",
    tradingdays.mauto       = "ANY",
    tradingdays.pftd        = "numeric",
    tradingdays.option      = "ANY",
    tradingdays.leapyear    = "logical",
    tradingdays.stocktd     = "integer",
    tradingdays.test        = "ANY",
    easter.type             = "ANY",
    easter.julian           = "logical",
    easter.duration         = "integer",
    easter.test             = "logical",
    outlier.enabled         = "logical",
    outlier.from            = "character",
    outlier.to              = "character",
    outlier.first           = "integer",
    outlier.last            = "integer",
    outlier.exclFirst       = "integer",
    outlier.exclLast        = "integer",
    outlier.ao              = "logical",
    outlier.tc              = "logical",
    outlier.ls              = "logical",
    outlier.so              = "logical",
    outlier.usedefcv        = "logical",
    outlier.cv              = "numeric",
    outlier.eml             = "logical",
    outlier.tcrate          = "numeric",
    automdl.enabled         = "logical",
    automdl.acceptdefault   = "logical",
    automdl.cancel          = "numeric",
    automdl.ub1             = "numeric",
    automdl.ub2             = "numeric",
    automdl.armalimit       = "numeric",
    automdl.reducecv        = "numeric",
    automdl.ljungboxlimit   = "numeric",
    automdl.compare         = "logical",
    arima.mu                = "logical",
    arima.p                 = "integer",
    arima.d                 = "integer",
    arima.q                 = "integer",
    arima.bp                = "integer",
    arima.bd                = "integer",
    arima.bq                = "integer",
    arima.coefEnabled       = "logical",
    arima.coef              = "ANY",
    arima.coefType          = "ANY",
    fcst.horizon            = "numeric",
    seats.predictionLength  = "integer",
    seats.approx            = "ANY",
    seats.trendBoundary     = "numeric",
    seats.seasdBoundary     = "numeric",
    seats.seasdBoundary1    = "numeric",
    seats.seasTol           = "numeric",
    seats.maBoundary        = "numeric",
    seats.method            = "ANY",
    ramps                   = "ANY",
    intervention_variables  = "ANY",
    easterCoef              = "ANY"
    #rampsCoef               = "ANY",
    #intervention_variablesCoef = "ANY"
    #tramoseats_spec         = "ANY"
  )
)


# Costruttore
#costruttore initialize per la classe
setMethod("initialize", "Extended_tramoseats_spec",
          function(.Object, series_name, frequency=NA_integer_, method ="TS" ,spec = "RSA0", preliminary.check = NA,
                   estimate.from = NA_character_, estimate.to = NA_character_,
                   estimate.first = NA_integer_, estimate.last = NA_integer_,
                   estimate.exclFirst = NA_integer_, estimate.exclLast = NA_integer_,
                   estimate.tol = NA_integer_, estimate.eml = NA, estimate.urfinal = NA_integer_,
                   transform.function = NA, transform.fct = NA_integer_,
                   usrdef.outliersEnabled = NA, usrdef.outliersType = NA,
                   usrdef.outliersDate = NA, usrdef.outliersCoef = NA,
                   userdef.varFromFile = FALSE, userdef.varFromFile.infoList = NULL,
                   usrdef.varEnabled = NA, usrdef.var = NA, usrdef.varType = NA,
                   usrdef.varCoef = NA, tradingdays.mauto = NA,
                   tradingdays.pftd = NA_integer_, tradingdays.option = NA,
                   tradingdays.leapyear = NA, tradingdays.stocktd = NA_integer_,
                   tradingdays.test = NA, easter.type = NA, easter.julian = NA,
                   easter.duration = NA_integer_, easter.test = NA, outlier.enabled = NA,
                   outlier.from = NA_character_, outlier.to = NA_character_,
                   outlier.first = NA_integer_, outlier.last = NA_integer_,
                   outlier.exclFirst = NA_integer_, outlier.exclLast = NA_integer_,
                   outlier.ao = NA, outlier.tc = NA, outlier.ls = NA, outlier.so = NA,
                   outlier.usedefcv = NA, outlier.cv = NA_integer_, outlier.eml = NA,
                   outlier.tcrate = NA_integer_, automdl.enabled = NA,
                   automdl.acceptdefault = NA, automdl.cancel = NA_integer_,
                   automdl.ub1 = NA_integer_, automdl.ub2 = NA_integer_,
                   automdl.armalimit = NA_integer_, automdl.reducecv = NA_integer_,
                   automdl.ljungboxlimit = NA_integer_, automdl.compare = NA,
                   arima.mu = NA, arima.p = NA_integer_, arima.d = NA_integer_,
                   arima.q = NA_integer_, arima.bp = NA_integer_, arima.bd = NA_integer_,
                   arima.bq = NA_integer_, arima.coefEnabled = NA, arima.coef = NA,
                   arima.coefType = NA, fcst.horizon = NA_integer_,
                   seats.predictionLength = NA_integer_, seats.approx = NA,
                   seats.trendBoundary = NA_integer_, seats.seasdBoundary = NA_integer_,
                   seats.seasdBoundary1 = NA_integer_, seats.seasTol = NA_integer_,
                   seats.maBoundary = NA_integer_, seats.method = NA, ramps=NA, intervention_variables=NA, #rampsCoef=NA, intervention_variablesCoef=NA, easterCoef=NA) {
                   easterCoef=0) {

            # Convert possible numeric arguments to integer if they are compatible
            integer_args_to_check <- c( "frequency", "estimate.first", "estimate.last", "estimate.exclFirst", "estimate.exclLast", "tradingdays.stocktd",
                                        "easter.duration", "outlier.first", "outlier.last", "outlier.exclFirst", "outlier.exclLast",
                                        "arima.p", "arima.d", "arima.q", "arima.bp", "arima.bd", "arima.bq",
                                        "seats.predictionLength")
            numeric_args_with_NA_integer <- c("estimate.tol", "transform.fct", "outlier.cv", "outlier.tcrate", "automdl.cancel", "automdl.ub1", "automdl.ub2", "automdl.armalimit", "automdl.reducecv", "automdl.ljungboxlimit", "fcst.horizon", "seats.trendBoundary", "seats.seasdBoundary", "seats.seasdBoundary1", "seats.seasTol", "seats.maBoundary")

            for (arg_name in integer_args_to_check)
            {
              if (!is.na(eval(parse(text = arg_name))) && is.numeric(eval(parse(text = arg_name))) && all(eval(parse(text = arg_name)) %% 1 == 0)) {
                assign(arg_name, as.integer(eval(parse(text = arg_name))))
              } else if (is.na(eval(parse(text = arg_name))))
              {
                assign(arg_name, NA_integer_)
              }
            }

            # Some numeric arguments have NA_integer_ as missing value. Sometimes simple NA is passed (i.e. when data are read from JSON),
            # so simple NAs have to be replaced with NA_integer_in some fields
            for (arg_name in numeric_args_with_NA_integer) {
              if (is.na(eval(parse(text = arg_name))))
              {
                assign(arg_name, NA_integer_)
              }
            }




            .Object@series_name <- series_name
            .Object@frequency   <- frequency
            .Object@method      <- method
            .Object@spec        <- spec

            # All the following rows are replaced by the dynamic evaluation below this commented rows
            # This is done because if an argument is not specified, we want the default given by the basic_spec (e.g. "RSA0") and not NA
            #
            # .Object@preliminary.check <- preliminary.check
            # .Object@estimate.from <- estimate.from
            # .Object@estimate.to <- estimate.to
            # .Object@estimate.first <- estimate.first
            # .Object@estimate.last <- estimate.last
            # .Object@estimate.exclFirst <- estimate.exclFirst
            # .Object@estimate.exclLast <- estimate.exclLast
            # .Object@estimate.tol <- estimate.tol
            # .Object@estimate.eml <- estimate.eml
            # .Object@estimate.urfinal <- estimate.urfinal
            # .Object@transform.function <- transform.function
            # .Object@transform.fct <- transform.fct
            # .Object@usrdef.outliersEnabled <- usrdef.outliersEnabled
            # .Object@usrdef.outliersType <- usrdef.outliersType
            # .Object@usrdef.outliersDate <- usrdef.outliersDate
            # .Object@usrdef.outliersCoef <- usrdef.outliersCoef
            # .Object@userdef.varFromFile <- userdef.varFromFile
            # .Object@userdef.varFromFile.infoList <- userdef.varFromFile.infoList
            # .Object@usrdef.varEnabled <- usrdef.varEnabled
            # .Object@usrdef.var <- usrdef.var
            # .Object@usrdef.varType <- usrdef.varType
            # .Object@usrdef.varCoef <- usrdef.varCoef
            # .Object@tradingdays.mauto <- tradingdays.mauto
            # .Object@tradingdays.pftd <- tradingdays.pftd
            # .Object@tradingdays.option <- tradingdays.option
            # .Object@tradingdays.leapyear <- tradingdays.leapyear
            # .Object@tradingdays.stocktd <- tradingdays.stocktd
            # .Object@tradingdays.test <- tradingdays.test
            # .Object@easter.type <- easter.type
            # .Object@easter.julian <- easter.julian
            # .Object@easter.duration <- easter.duration
            # .Object@easter.test <- easter.test
            # .Object@outlier.enabled <- outlier.enabled
            # .Object@outlier.from <- outlier.from
            # .Object@outlier.to <- outlier.to
            # .Object@outlier.first <- outlier.first
            # .Object@outlier.last <- outlier.last
            # .Object@outlier.exclFirst <- outlier.exclFirst
            # .Object@outlier.exclLast <- outlier.exclLast
            # .Object@outlier.ao <- outlier.ao
            # .Object@outlier.tc <- outlier.tc
            # .Object@outlier.ls <- outlier.ls
            # .Object@outlier.so <- outlier.so
            # .Object@outlier.usedefcv <- outlier.usedefcv
            # .Object@outlier.cv <- outlier.cv
            # .Object@outlier.eml <- outlier.eml
            # .Object@outlier.tcrate <- outlier.tcrate
            # .Object@automdl.enabled <- automdl.enabled
            # .Object@automdl.acceptdefault <- automdl.acceptdefault
            # .Object@automdl.cancel <- automdl.cancel
            # .Object@automdl.ub1 <- automdl.ub1
            # .Object@automdl.ub2 <- automdl.ub2
            # .Object@automdl.armalimit <- automdl.armalimit
            # .Object@automdl.reducecv <- automdl.reducecv
            # .Object@automdl.ljungboxlimit <- automdl.ljungboxlimit
            # .Object@automdl.compare <- automdl.compare
            # .Object@arima.mu <- arima.mu
            # .Object@arima.p <- arima.p
            # .Object@arima.d <- arima.d
            # .Object@arima.q <- arima.q
            # .Object@arima.bp <- arima.bp
            # .Object@arima.bd <- arima.bd
            # .Object@arima.bq <- arima.bq
            # .Object@arima.coefEnabled <- arima.coefEnabled
            # .Object@arima.coef <- arima.coef
            # .Object@arima.coefType <- arima.coefType
            # .Object@fcst.horizon <- fcst.horizon
            # .Object@seats.predictionLength <- seats.predictionLength
            # .Object@seats.approx <- seats.approx
            # .Object@seats.trendBoundary <- seats.trendBoundary
            # .Object@seats.seasdBoundary <- seats.seasdBoundary
            # .Object@seats.seasdBoundary1 <- seats.seasdBoundary1
            # .Object@seats.seasTol <- seats.seasTol
            # .Object@seats.maBoundary <- seats.maBoundary
            # .Object@seats.method <- seats.method

            # Set the default value as it is in the basic_spec (e.g. "RSA0") instead of NA if an argument is not specified
            attributes <- c("preliminary.check", "estimate.from", "estimate.to",
                            "estimate.first", "estimate.last", "estimate.exclFirst",
                            "estimate.exclLast", "estimate.tol", "estimate.eml",
                            "estimate.urfinal", "transform.function", "transform.fct",
                            "usrdef.outliersEnabled", "usrdef.outliersType",
                            "usrdef.outliersDate", "usrdef.outliersCoef", "userdef.varFromFile",
                            "userdef.varFromFile.infoList", "usrdef.varEnabled",
                            "usrdef.var", "usrdef.varType", "usrdef.varCoef",
                            "tradingdays.mauto", "tradingdays.pftd", "tradingdays.option",
                            "tradingdays.leapyear", "tradingdays.stocktd",
                            "tradingdays.test", "easter.type", "easter.julian",
                            "easter.duration", "easter.test", "outlier.enabled",
                            "outlier.from", "outlier.to", "outlier.first", "outlier.last",
                            "outlier.exclFirst", "outlier.exclLast", "outlier.ao",
                            "outlier.tc", "outlier.ls", "outlier.so", "outlier.usedefcv",
                            "outlier.cv", "outlier.eml", "outlier.tcrate", "automdl.enabled",
                            "automdl.acceptdefault", "automdl.cancel", "automdl.ub1",
                            "automdl.ub2", "automdl.armalimit", "automdl.reducecv",
                            "automdl.ljungboxlimit", "automdl.compare", "arima.mu",
                            "arima.p", "arima.d", "arima.q", "arima.bp", "arima.bd", "arima.bq",
                            "arima.coefEnabled", "arima.coef", "arima.coefType", "fcst.horizon",
                            "seats.predictionLength", "seats.approx", "seats.trendBoundary",
                            "seats.seasdBoundary", "seats.seasdBoundary1", "seats.seasTol",
                            "seats.maBoundary", "seats.method", "ramps", "intervention_variables", #"rampsCoef", "intervention_variablesCoef", "easterCoef")
                            "easterCoef")

            #basic_spec <- get_basic_spec("RSA0")
            basic_spec <- get_basic_spec(spec)

            for (attr in attributes)
            {
              attr_value <- eval(parse(text = attr))
              if (!all(is.na(attr_value)))
              {
                slot(.Object, attr) <- attr_value
              }else
              {
                if(attr %in% integer_args_to_check)
                {
                  basic_spec[[attr]]<-as.integer(basic_spec[[attr]])
                }
                slot(.Object, attr) <- basic_spec[[attr]]
              }
            }

            return(.Object)
          })




# R-like Constructor:
# Helper function for creating an Extended_tramoseats object
Extended_tramoseats_spec_helper <- function(series_name = NULL, frequency=NA_integer_, method="TS" ,spec = NULL, preliminary.check = NA,
                                            estimate.from = NA_character_, estimate.to = NA_character_,
                                            estimate.first = NA_integer_, estimate.last = NA_integer_,
                                            estimate.exclFirst = NA_integer_, estimate.exclLast = NA_integer_,
                                            estimate.tol = NA_integer_, estimate.eml = NA,
                                            estimate.urfinal = NA_integer_, transform.function = NA,
                                            transform.fct = NA_integer_, usrdef.outliersEnabled = NA,
                                            usrdef.outliersType = NA, usrdef.outliersDate = NA,
                                            usrdef.outliersCoef = NA, userdef.varFromFile = FALSE,
                                            userdef.varFromFile.infoList = NULL, usrdef.varEnabled = NA,
                                            usrdef.var = NA, usrdef.varType = NA, usrdef.varCoef = NA,
                                            tradingdays.mauto = NA, tradingdays.pftd = NA_integer_,
                                            tradingdays.option = NA, tradingdays.leapyear = NA,
                                            tradingdays.stocktd = NA_integer_, tradingdays.test = NA,
                                            easter.type = NA, easter.julian = NA, easter.duration = NA_integer_,
                                            easter.test = NA, outlier.enabled = NA, outlier.from = NA_character_,
                                            outlier.to = NA_character_, outlier.first = NA_integer_,
                                            outlier.last = NA_integer_, outlier.exclFirst = NA_integer_,
                                            outlier.exclLast = NA_integer_, outlier.ao = NA, outlier.tc = NA,
                                            outlier.ls = NA, outlier.so = NA, outlier.usedefcv = NA,
                                            outlier.cv = NA_integer_, outlier.eml = NA, outlier.tcrate = NA_integer_,
                                            automdl.enabled = NA, automdl.acceptdefault = NA,
                                            automdl.cancel = NA_integer_, automdl.ub1 = NA_integer_,
                                            automdl.ub2 = NA_integer_, automdl.armalimit = NA_integer_,
                                            automdl.reducecv = NA_integer_, automdl.ljungboxlimit = NA_integer_,
                                            automdl.compare = NA, arima.mu = NA, arima.p = NA_integer_,
                                            arima.d = NA_integer_, arima.q = NA_integer_, arima.bp = NA_integer_,
                                            arima.bd = NA_integer_, arima.bq = NA_integer_,
                                            arima.coefEnabled = NA, arima.coef = NA, arima.coefType = NA,
                                            fcst.horizon = NA_integer_, seats.predictionLength = NA_integer_,
                                            seats.approx = NA, seats.trendBoundary = NA_integer_,
                                            seats.seasdBoundary = NA_integer_, seats.seasdBoundary1 = NA_integer_,
                                            seats.seasTol = NA_integer_, seats.maBoundary = NA_integer_,
                                            seats.method = NA, ramps = NA, intervention_variables = NA, #rampsCoef=NA, intervention_variablesCoef=NA, easterCoef=NA) {
                                            easterCoef=0) {

  new("Extended_tramoseats_spec", series_name = series_name, frequency=frequency, method=method, spec = spec, preliminary.check = preliminary.check,
      estimate.from = estimate.from, estimate.to = estimate.to, estimate.first = estimate.first,
      estimate.last = estimate.last, estimate.exclFirst = estimate.exclFirst, estimate.exclLast = estimate.exclLast,
      estimate.tol = estimate.tol, estimate.eml = estimate.eml, estimate.urfinal = estimate.urfinal,
      transform.function = transform.function, transform.fct = transform.fct,
      usrdef.outliersEnabled = usrdef.outliersEnabled, usrdef.outliersType = usrdef.outliersType,
      usrdef.outliersDate = usrdef.outliersDate, usrdef.outliersCoef = usrdef.outliersCoef,
      userdef.varFromFile = userdef.varFromFile, userdef.varFromFile.infoList = userdef.varFromFile.infoList,
      usrdef.varEnabled = usrdef.varEnabled, usrdef.var = usrdef.var, usrdef.varType = usrdef.varType,
      usrdef.varCoef = usrdef.varCoef, tradingdays.mauto = tradingdays.mauto, tradingdays.pftd = tradingdays.pftd,
      tradingdays.option = tradingdays.option, tradingdays.leapyear = tradingdays.leapyear,
      tradingdays.stocktd = tradingdays.stocktd, tradingdays.test = tradingdays.test,
      easter.type = easter.type, easter.julian = easter.julian, easter.duration = easter.duration,
      easter.test = easter.test, outlier.enabled = outlier.enabled, outlier.from = outlier.from,
      outlier.to = outlier.to, outlier.first = outlier.first, outlier.last = outlier.last,
      outlier.exclFirst = outlier.exclFirst, outlier.exclLast = outlier.exclLast, outlier.ao = outlier.ao,
      outlier.tc = outlier.tc, outlier.ls = outlier.ls, outlier.so = outlier.so,
      outlier.usedefcv = outlier.usedefcv, outlier.cv = outlier.cv, outlier.eml = outlier.eml,
      outlier.tcrate = outlier.tcrate, automdl.enabled = automdl.enabled,
      automdl.acceptdefault = automdl.acceptdefault, automdl.cancel = automdl.cancel,
      automdl.ub1 = automdl.ub1, automdl.ub2 = automdl.ub2, automdl.armalimit = automdl.armalimit,
      automdl.reducecv = automdl.reducecv, automdl.ljungboxlimit = automdl.ljungboxlimit,
      automdl.compare = automdl.compare, arima.mu = arima.mu, arima.p = arima.p,
      arima.d = arima.d, arima.q = arima.q, arima.bp = arima.bp, arima.bd = arima.bd,
      arima.bq = arima.bq, arima.coefEnabled = arima.coefEnabled, arima.coef = arima.coef,
      arima.coefType = arima.coefType, fcst.horizon = fcst.horizon,
      seats.predictionLength = seats.predictionLength, seats.approx = seats.approx,
      seats.trendBoundary = seats.trendBoundary, seats.seasdBoundary = seats.seasdBoundary,
      seats.seasdBoundary1 = seats.seasdBoundary1, seats.seasTol = seats.seasTol,
      seats.maBoundary = seats.maBoundary, seats.method = seats.method, ramps = ramps, intervention_variables = intervention_variables, #rampsCoef=rampsCoef, intervention_variablesCoef=intervention_variablesCoef, easterCoef=easterCoef)
      easterCoef=easterCoef)
}

# Definition of the function alias (costruttore R-like)
Extended_tramoseats_spec <- Extended_tramoseats_spec_helper







# Definition of the method to_JD_JSON #If I define it like this, I am not able to call it in from_reduced_to_full_JD_JSON_file di JD_JSON.r, thus I do the normal definition
# default basic_spec "RSA0"
setGeneric("to_JD_JSON", function(object, indent = FALSE, diff = TRUE, basic_spec="RSA0") standardGeneric("to_JD_JSON"))
setMethod("to_JD_JSON", "Extended_tramoseats_spec", function(object, indent = FALSE, diff = TRUE, basic_spec="RSA0") {
  #to_JD_JSON <- function(object, indent = FALSE, diff = TRUE)
  #{
  require(rjson)
  require(RJDemetra)


  # browser()
  if(diff == TRUE)
  {


    #browser()
    basic_spec_instance   <- from_SA_spec(SA_spec = tramoseats_spec(basic_spec), method="TS", userdef.varFromFile = FALSE)

    object       <- difference_objects_preserving_name_and_spec(object, basic = basic_spec_instance)
  }
  else
  {
    object <- to_named_list(object)
  }

  # Convert the object in JSON using rjson
  json_spec <- rjson::toJSON(object)#, indent = indent)

  if(indent == TRUE)
  {
    # Aggiungi andare a capo dopo ogni coppia chiave-valore che termina per "
    json_spec <- gsub("\\{\\s*\"", "{\n\"", json_spec)
    json_spec <- gsub("\",\\s*", "\",\n", json_spec)
    # Aggiungi andare a capo dopo i valori numerici e booleani
    json_spec <- gsub("([0-9]+|true|false),", "\\1,\n", json_spec)
    # Aggiungi andare a capo dopo la parentesi quadra seguita dalla virgola
    json_spec <- gsub("\\],", "\\],\n", json_spec)
    # Aggiungi andare a capo prima delle parentesi graffe chiuse, a meno che non siano seguite da una parentesi quadra chiusa
    json_spec <- gsub("\\}(?!\\])", "\n}", json_spec, perl = TRUE) # without PERL: json_spec <- gsub("}([^\\]])", "\n}\\1", json_spec)
    # Aggiungi degli spazi dopo le virgole, ma solo quando non sono seguite da un carattere di nuova riga
    json_spec <- gsub(",([^\\r\\n])", ", \\1", json_spec)


    require(stringr)
    vec_pattern <- "\\[[^\\[\\{\\}]*?\\]"
    json_spec <- str_replace_all(json_spec, vec_pattern, function(match) {
      # Manipola il vettore isolato
      # In questo caso, possiamo rimuovere gli andare a capo all'interno del vettore
      match <- gsub("[\r\n]", "", match)
      return(match)
    })
  }

  return(json_spec)
})




setGeneric("from_JD_JSON", function(object, json) standardGeneric("from_JD_JSON"))
setMethod ("from_JD_JSON", "Extended_tramoseats_spec", function(object, json) {
  require(rjson)
  json_list <- fromJSON(json)

  # Inizializza gli argomenti per il costruttore
  args <- list(
    series_name = json_list$series_name,
    frequency   = frequency,
    method = json_list$method,
    spec = json_list$spec,
    preliminary.check = json_list$preliminary.check,
    estimate.from = json_list$estimate.from,
    estimate.to = json_list$estimate.to,
    estimate.first = json_list$estimate.first,
    estimate.last = json_list$estimate.last,
    estimate.exclFirst = json_list$estimate.exclFirst,
    estimate.exclLast = json_list$estimate.exclLast,
    estimate.tol = json_list$estimate.tol,
    estimate.eml = json_list$estimate.eml,
    estimate.urfinal = json_list$estimate.urfinal,
    transform.function = json_list$transform.function,
    transform.fct = json_list$transform.fct,
    usrdef.outliersEnabled = json_list$usrdef.outliersEnabled,
    usrdef.outliersType = json_list$usrdef.outliersType,
    usrdef.outliersDate = json_list$usrdef.outliersDate,
    usrdef.outliersCoef = json_list$usrdef.outliersCoef,
    userdef.varFromFile = json_list$userdef.varFromFile,
    userdef.varFromFile.infoList = json_list$userdef.varFromFile.infoList,
    usrdef.varEnabled = json_list$usrdef.varEnabled,
    usrdef.var = json_list$usrdef.var,
    usrdef.varType = json_list$usrdef.varType,
    usrdef.varCoef = json_list$usrdef.varCoef,
    tradingdays.mauto = json_list$tradingdays.mauto,
    tradingdays.pftd = json_list$tradingdays.pftd,
    tradingdays.option = json_list$tradingdays.option,
    tradingdays.leapyear = json_list$tradingdays.leapyear,
    tradingdays.stocktd = json_list$tradingdays.stocktd,
    tradingdays.test = json_list$tradingdays.test,
    easter.type = json_list$easter.type,
    easter.julian = json_list$easter.julian,
    easter.duration = json_list$easter.duration,
    easter.test = json_list$easter.test,
    outlier.enabled = json_list$outlier.enabled,
    outlier.from = json_list$outlier.from,
    outlier.to = json_list$outlier.to,
    outlier.first = json_list$outlier.first,
    outlier.last = json_list$outlier.last,
    outlier.exclFirst = json_list$outlier.exclFirst,
    outlier.exclLast = json_list$outlier.exclLast,
    outlier.ao = json_list$outlier.ao,
    outlier.tc = json_list$outlier.tc,
    outlier.ls = json_list$outlier.ls,
    outlier.so = json_list$outlier.so,
    outlier.usedefcv = json_list$outlier.usedefcv,
    outlier.cv = json_list$outlier.cv,
    outlier.eml = json_list$outlier.eml,
    outlier.tcrate = json_list$outlier.tcrate,
    automdl.enabled = json_list$automdl.enabled,
    automdl.acceptdefault = json_list$automdl.acceptdefault,
    automdl.cancel = json_list$automdl.cancel,
    automdl.ub1 = json_list$automdl.ub1,
    automdl.ub2 = json_list$automdl.ub2,
    automdl.armalimit = json_list$automdl.armalimit,
    automdl.reducecv = json_list$automdl.reducecv,
    automdl.ljungboxlimit = json_list$automdl.ljungboxlimit,
    automdl.compare = json_list$automdl.compare,
    arima.mu = json_list$arima.mu,
    arima.p = json_list$arima.p,
    arima.d = json_list$arima.d,
    arima.q = json_list$arima.q,
    arima.bp = json_list$arima.bp,
    arima.bd = json_list$arima.bd,
    arima.bq = json_list$arima.bq,
    arima.coefEnabled = json_list$arima.coefEnabled,
    arima.coef = json_list$arima.coef,
    arima.coefType = json_list$arima.coefType,
    fcst.horizon = json_list$fcst.horizon,
    seats.predictionLength = json_list$seats.predictionLength,
    seats.approx = json_list$seats.approx,
    seats.trendBoundary = json_list$seats.trendBoundary,
    seats.seasdBoundary = json_list$seats.seasdBoundary,
    seats.seasdBoundary1 = json_list$seats.seasdBoundary1,
    seats.seasTol = json_list$seats.seasTol,
    seats.maBoundary = json_list$seats.maBoundary,
    seats.method = json_list$seats.method,
    ramps = json_list$ramps,
    intervention_variables = json_list$intervention_variables
  )

  # Removes arguments not specified
  args <- args[sapply(args, Negate(is.null))]

  # Build a new Extended_tramoseats_spec object
  do.call("Extended_tramoseats_spec", args)

})

















to_named_list <- function(object) {
  # Obtain slot names for S4 object
  slot_names <- slotNames(class(object))

  # Initialize an empty named list
  result_list <- list()

  # Add slot values to named list using slot()
  for (slot_name in slot_names) {
    result_list[[slot_name]] <- slot(object, slot_name)
  }

  return(result_list)
}


extended_tramoseats_spec_list_from_workspace <-  function(workspace, data_reader_ext_reg, method="TS", java_processing=FALSE ,...)
{
  require(RJDemetra)

  compute(workspace)

  #jmodel          <- RJDemetra::get_jmodel(workspace, progress_bar = TRUE) # to retrieve external regressors by name
  cat("Loading models\n")


  ##browser()
  ##This part of code should be optimized (always do java)
  # if(java_processing == FALSE)
  # {
  jm <- get_jmodel(workspace, progress_bar = FALSE) #added later: jmodel is necessary in any case for ramps and intervention variables
  suppressWarnings(m  <- get_model(workspace, progress_bar = TRUE)) # Suppress warning here??
  # }else
  # {
  #   #browser()
  #   m  <- get_jmodel(workspace, progress_bar = TRUE)
  #   jm <- m
  #   m  <- get_r_model_from_j_model(m)
  # }

  cat("Loading external variables\n")
  ##browser()
  all_model_vars_info <- data_reader_ext_reg@read_ext_reg_info(workspace)

  #all_jmodel_vars <- getUserDefinedTdVariables_info(jmodel) # per editare la scrittura

  #browser()

  extended_tramoseats_spec_list <- list()

  for (series_name in names(m[[1]]))
  {
    easterCoef <- 0

    series        <-  m[[1]][[series_name]]
    #browser()
    if(is.null(m[[1]][[series_name]])) # due to ramps or Intervention variable...
    {
      #... try to load series from jm

      message("Some series have value NULL in the R object, they are taken from Java objects")

      ts              <- jm[[1]][[series_name]]
      time_series_obj <- get_indicators(x = ts, "y")[[1]]

      message(paste("Series", series_name,"has been found in Java object;"))
      message("despite the some warnings-(non-blocking)errors below, the JD_JSON has been producted correctly!!")
      message("(you can use it for the processing on Java Servers or in RJDProcessor, for example)")
      message("Workspaces produced by RJDProcessor are OK for the GUI application")
      message("Warning-(non-blocking)errors are due to missing RAMP and INTERVENTION_VARIABLE support in RJDemetra")
      message("Because of this some plots won't be produces or you must use get_jmodel instead of get_model in RJDemetra")


      frequency     <-  frequency(time_series_obj)
      basic_spec    <-  jm[[1]][[series_name]]$spec$getCore()$toString()
      #if(basic_spec=="TS") { basic_spec<-"RSA0" } #TS=custom spec --> by default set "RSA0" #encoded in the constructor call of Extended_tramoseats_spec

      #browser()
      series   <- jm[[1]][[series_name]]#$spec #$getCore()




      ##########################################


      # READING OF FIXED COEFFICIENTS (FOR EASTER, LY, RAMPS AND INTERVENTION_VARS) #
      # see also the part before
      ##### for additional fields handling #####
      # series_j <- jm[[1]][[series_name]]$spec
      # Ramp                 <- J("ec/tstoolkit/timeseries/regression/Ramp")
      # InterventionVariable <- J("ec/tstoolkit/timeseries/regression/InterventionVariable")
      # ArrayList            <- J("java/util/ArrayList")
      # empty_ramps                  <- .jarray(list(), contents.class = "ec/tstoolkit/timeseries/regression/Ramp")
      # empty_intervention_variables <- .jarray(list(), contents.class = "ec/tstoolkit/timeseries/regression/InterventionVariable")
      # jSA_series      <- jmodel[[1]][[name]]
      # jRegression     <- jSA_series$spec$getRegression()
      # jCalendar       <- jRegression$getCalendar()
      # jTradingDays    <- jCalendar$getTradingDays()
      # jUser_Td_VarsString      <- jTradingDays$getUserVariables()
      # file_list <- list()
      #
      # coef_list <- list() #Added
      #
      # idx_file_list = 1
      #
      # usrDefVarCount    <- jRegression$getUserDefinedVariablesCount()

      #browser()

      if(!is.null(series_name) && !is.na(series_name) && !is.null(workspace))
      {
        #jm           <- get_jmodel(workspace)
        #series_j     <- jm[[1]][[series_name]]$spec
        series_j     <- ts$spec
        jRegression  <- series_j$getCore()$getTramoSpecification()$getRegression()
        if(jRegression$hasFixedCoefficients())
        {
          fixed_coef <- jRegression$getAllFixedCoefficients()

          jRegression$getFixedCoefficients("easter")
          if(!is.null(jRegression$getFixedCoefficients("easter")) && !is.na(jRegression$getFixedCoefficients("easter")))
          {
            easterCoef <- jRegression$getFixedCoefficients("easter")
          }


          rps <- get_ramps_from_java(ts)
          if(length(rps)>0)
          {
            #browser()
            for (rp in rps)
            {
              #browser()
              all_model_vars_info$ramps[[series_name]] <- lapply(all_model_vars_info$ramps[[series_name]], function(ramp) {
                if (ramp$start == rp$start && ramp$end == rp$end) {  if(!is.null(jRegression$getFixedCoefficients(rp$name))){ramp$fixed_coef <- jRegression$getFixedCoefficients(rp$name)} else {ramp$fixed_coef <- 0} } #{ramp$fixed_coef <- NA}   }
                    return(ramp)  })
            }

          }

          ivs <- get_intervention_variables_from_java(ts)
        #   if(length(ivs)>0)
        #   {
        #     #browser()
        #     for (iv in ivs)
        #     {
        #       #browser()
        #
        #       # correspondingSequence = function that returns TRUE if the sequence of intervention_var is the same as the sequence of an iv
        #       all_model_vars_info$intervention_vars[[series_name]] <- lapply(all_model_vars_info$intervention_vars[[series_name]], function(intervention_var) {
        #         if(correspondingSequence && intervention_var$delta == iv$delta && intervention_var$delta_s == iv$delta_s && intervention_var$D1DS == iv$D1DS) {  if(!is.null(jRegression$getFixedCoefficients(iv$name))){intervention_var$fixed_coef <- jRegression$getFixedCoefficients(iv$name)} else {iv$fixed_coef <- 0} } #{iv$fixed_coef <- NA}   }
        #         return(intervention_var)  })
        #     }
        #
        #   }
        #


        }


      }



      ##############################################################################


      # Fundamental to make everything works!
      series_j <- jm[[1]][[series_name]]$spec
      Ramp                 <- J("ec/tstoolkit/timeseries/regression/Ramp")
      InterventionVariable <- J("ec/tstoolkit/timeseries/regression/InterventionVariable")
      ArrayList            <- J("java/util/ArrayList")
      empty_ramps                  <- .jarray(list(), contents.class = "ec/tstoolkit/timeseries/regression/Ramp")
      empty_intervention_variables <- .jarray(list(), contents.class = "ec/tstoolkit/timeseries/regression/InterventionVariable")

      series_j$getCore()$getTramoSpecification()$getRegression()$setRamps(empty_ramps)
      series_j$getCore()$getTramoSpecification()$getRegression()$setInterventionVariables(empty_intervention_variables)
      # series_j$getCore()$getTramoSpecification()$getRegression()$setRamps(NULL) # place an empty arraylist()?
      # series_j$getCore()$getTramoSpecification()$getRegression()$setInterventionVariables(NULL) # place an empty arraylist()?
      series$spec <- series_j

      series   <- jSA2R(x = series)
    } else # series in R, not everything is available in R, e.g. easter Fixed Coefficient
    {
      #browser()
      frequency     <-  frequency(get_ts(series))

      jRegression<-jm[[1]][[series_name]]$spec$getCore()$getTramoSpecification()$getRegression()
      if(jRegression$hasFixedCoefficients())
      {
        if(!is.null(jRegression$getFixedCoefficients("easter")) && !is.na(jRegression$getFixedCoefficients("easter")))
        {
          easterCoef <- jRegression$getFixedCoefficients("easter")
        }
      }

      # Suppress warning here?
      suppressWarnings({
        #browser()
        basic_spec    <- get_jspec(jm[[1]][[series_name]])$toString()
      })

      #if(basic_spec=="TS") { basic_spec<-"RSA0" } #TS=custom spec --> by default set "RSA0" #encoded in the constructor call of Extended_tramoseats_spec
    }

    #asd
    #browser()
    spec <- from_SA_spec(series, series_name = series_name, frequency = frequency, method = method, basic_spec=basic_spec, all_model_vars_info = all_model_vars_info, data_reader_ext_reg = data_reader_ext_reg, workspace = workspace, easterCoef=easterCoef)
    spec <- list(spec)
    extended_tramoseats_spec_list <- append(extended_tramoseats_spec_list ,spec)
  }
  #browser()
  gc() # to clean memory after the use of rJava

  return(extended_tramoseats_spec_list)

}




from_SA_spec <- function(SA_spec, series_name = NA_character_, frequency = NA_integer_  ,method = "TS" ,basic_spec="RSA0", userdef.varFromFile=TRUE, all_model_vars_info=NULL, data_reader_ext_reg=NULL ,workspace=NA, easterCoef=0)#NA)
{
  require(RJDemetra)

  ##### for additional fields handling #####
  #browser()
  ramps                  = NA
  intervention_variables = NA
  easterCoef            = easterCoef
  #rampsCoef             = NA
  #intervention_variablesCoef = NA
  #leap_yearCoef         = NA

  suppressWarnings(NA_workspace <- is.na(workspace))
  if(!is.null(workspace) && !NA_workspace) # for the basic_specs there is no workspace
  {
    #browser()

    #all_model_vars_info = data_reader_ext_reg@read_ext_reg_info(workspace)

    #browser()  # Move the istructions to retrieve all_model_vars_info into this function instead of the Data_reader_ext_reg?
    all_model_ivs_info        <- all_model_vars_info[["intervention_vars"]]
    all_model_ramps_info      <- all_model_vars_info[["ramps"]]
    all_model_ext_vars_info   <- all_model_vars_info[["ext_vars"]]
    all_model_usrDef_var_coef <- all_model_vars_info[["varCoef"]]

    no_ramps <- all(sapply(all_model_ramps_info, function(x) length(x) == 0))
    no_ivs   <- all(sapply(all_model_ivs_info  , function(x) length(x) == 0))
    no_coef  <- (all(sapply(all_model_usrDef_var_coef  , function(x) length(x) == 0)) || all(unlist(all_model_usrDef_var_coef) == 0))

    if(!(no_ramps && no_ivs))
    {

      # cat("\n\n_____________________________________________________\n")
      # cat(paste("time series:", series_name, "\n" ))
      # print("WARNING: In your JDemetra+ Workspace RAMPS or INTERVENTION VARIABLES are used")
      # print("Unfortunately RJDProcessor is not able to handle them now in producing results")
      # print("However you can produce JSONs for Java Servers")
      # #print("SUGGESTION: replace them with External Variables with the same values to obtain similar results")
      # cat("_____________________________________________________\n\n")

      if(!no_ramps)
      {
        ramps =  all_model_ramps_info[[series_name]]

        # To add rampsCoef as separate fields in JD_JSON, but we want to pass the coefficients in the ramo_info JSON object, together with start and end
        # if (length(ramps) > 0)
        # {
        #   rampsCoef <- numeric(0)  # inizializza il vettore vuoto
        #   for (r in ramps)
        #   {
        #       rampsCoef <- append(rampsCoef, r$fixed_coef)  # aggiungi l'elemento a rampsCoef
        #   }
        # }


      }
      if(!no_ivs)
      {
        intervention_variables =  all_model_ivs_info[[series_name]]
      }

    }
  }



  #browser()
  if(!is.null(SA_spec$regarima$specification))#added for diff #tramoseats_spec object
  {
    regarima_spec <- SA_spec$regarima$specification
    seats_spec    <- SA_spec$decomposition$specification
  } else #regarima object
  {
    regarima_spec <- SA_spec$regarima
    seats_spec    <- SA_spec$seats
  }

  if(nrow(regarima_spec$estimate) == 3)
  {
    regarima_spec <- simplify_leaves(regarima_spec)
    seats_spec    <- simplify_leaves(seats_spec)
  }


  estimate_span_details <- span_unpack_into_spec(span = regarima_spec$estimate$span)
  outliers_span_details <- span_unpack_into_spec(span = regarima_spec$outliers$span)


  if(userdef.varFromFile == TRUE)
  {
    vars_mts       <- NA #ts()
    usrdef.varType <- NA
    usrdef.varCoef <- NA
    userdef.varFromFile.infoList <- NULL

    if(!is.null(all_model_ext_vars_info))
    {
      if(is.na(series_name))
      {
        warning("Impossible to read external variables without specifying series name! The procedure ends without considering extrernal variables", call=TRUE)
      }else
      {
        vars_mts          <- data_reader_ext_reg@read_ext_reg_data(all_model_ext_vars_info, series_name, frequency=frequency) #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        user_def_var_info <- get_user_def_var_info(regarima_spec) # si può prendere anche dal workspace?
        usrdef.varType    <- user_def_var_info$type
        #usrdef.varCoef    <- user_def_var_info$coef
        if(!is.na(series_name) && !all(all_model_vars_info[["varCoef"]][[series_name]]==0))
        {
          usrdef.varCoef <- all_model_vars_info[["varCoef"]][[series_name]]
        }else
        {
          usrdef.varCoef <- NA
        }

        userdef.varFromFile.infoList = all_model_ext_vars_info[[series_name]]
        #browser()
        if (!inherits(vars_mts, c("ts", "mts")) && is.na(vars_mts))  {  userdef.varFromFile = FALSE  } #se è un problema togliere questa riga
      }
    }
  }
  else # userdef.varFromFile == FALSE  (metterle prima dell'if come inizializzazione per maggiore chiarezza?)
  {
    # TODO
    vars_mts <- ts()      #SA_spec$...
    usrdef.varType <- NA  #get_user_def_var_info$Type ?
    # usrdef.varCoef <- NA  #get_user_def_var_info$Coef ?
    if(!is.na(series_name)) #the basic_specs have series_name=NA
    {
      usrdef.varCoef <- all_model_vars_info[["varCoef"]][[series_name]]
    }else
    {
      usrdef.varCoef <- NA
    }

    userdef.varFromFile.infoList <- NULL
  }


  extended_tramoseats_spec <- Extended_tramoseats_spec(
    spec               = ifelse(basic_spec=="TS", "RSA0", basic_spec),
    series_name        = series_name, # custom field
    frequency          = frequency,   # custom field
    method             = method,      # custom field
    preliminary.check  = regarima_spec$estimate$preliminary.check,
    estimate.from      = estimate_span_details$from,      #regarima_spec$span$d1[1],
    estimate.to        = estimate_span_details$to,        #regarima_spec$span$d0[1],
    estimate.first     = estimate_span_details$first,     #ifelse(!is.na(regarima_spec$span$d1[1]) || !is.na(regarima_spec$span$d0[1]) || !is.na(regarima_spec$span$n1[1]),NA_integer_,regarima_spec$span$n0[1]),
    estimate.last      = estimate_span_details$last,      #ifelse(!is.na(regarima_spec$span$d1[1]) || !is.na(regarima_spec$span$d0[1]), NA_integer_, regarima_spec$span$n1[1]),
    estimate.exclFirst = estimate_span_details$exclFirst, #ifelse((!is.na(regarima_spec$span$d1[1]) || !is.na(regarima_spec$span$d0[1]) || !is.na(regarima_spec$span$n0[1]) || !is.na(regarima_spec$span$n1[1])) && regarima_spec$span$type[1]!="All",  0, regarima_spec$span$n0[1]),
    estimate.exclLast  = estimate_span_details$exclLast,  #ifelse((!is.na(regarima_spec$span$d1[1]) || !is.na(regarima_spec$span$d0[1]) || !is.na(regarima_spec$span$n0[1]) || !is.na(regarima_spec$span$n1[1])) && regarima_spec$span$type[1]!="All",  0, regarima_spec$span$n1[1]),
    estimate.tol       = regarima_spec$estimate$tolerance,
    estimate.eml       = regarima_spec$estimate$exact_ml,
    estimate.urfinal   = regarima_spec$estimate$urfinal,
    transform.function = regarima_spec$transform$tfunction,
    transform.fct      = regarima_spec$transform$fct,
    usrdef.outliersEnabled = regarima_spec$regression$userdef$specification$outlier,
    usrdef.outliersType    = get_outliers_info(regarima_spec)$type, #not working ->ifelse(!is.null(regarima_spec$regression$userdef$outliers$type), regarima_spec$regression$userdef$outliers$type, NA),
    usrdef.outliersDate    = get_outliers_info(regarima_spec)$date,  #ifelse(regarima_spec$regression$userdef$specification$outlier==TRUE, regarima_spec$regression$userdef$outliers$date, NA),
    usrdef.outliersCoef    = get_outliers_info(regarima_spec)$coeff, #ifelse(regarima_spec$regression$userdef$specification$outlier.coef, regarima_spec$regression$userdef$outliers$coeff,NA),
    usrdef.varEnabled      = regarima_spec$regression$userdef$specification$variables,
    userdef.varFromFile    = userdef.varFromFile,
    userdef.varFromFile.infoList = userdef.varFromFile.infoList,
    usrdef.var             = ifelse(userdef.varFromFile==TRUE, NA,vars_mts), #get_userdef_var(userdef.varFromFile, vars_mts, regarima_spec, series_name),#, # !!!
    usrdef.varType         = usrdef.varType, #ifelse(is.na(vars_mts), NA, regarima_spec$regression$userdef$variables$description$type), # prima era "Undefined"
    usrdef.varCoef         = usrdef.varCoef, #ifelse(regarima_spec$regression$userdef$specification$variables.coef, regarima_spec$regression$userdef$variables$description$coeff, NA),
    tradingdays.mauto    = regarima_spec$regression$trading.days$automatic,
    tradingdays.pftd     = regarima_spec$regression$trading.days$pftd,
    tradingdays.option   = regarima_spec$regression$trading.days$option,
    tradingdays.leapyear = regarima_spec$regression$trading.days$leapyear,
    tradingdays.stocktd  = regarima_spec$regression$trading.days$stocktd,
    tradingdays.test     = regarima_spec$regression$trading.days$test,
    easter.type          = regarima_spec$regression$easter$type,
    easter.julian        = regarima_spec$regression$easter$julian,
    easter.duration      = regarima_spec$regression$easter$duration,
    easter.test          = regarima_spec$regression$easter$test,
    outlier.enabled   = regarima_spec$outliers$enabled,
    outlier.from      = outliers_span_details$from,      #regarima_spec$span$d1[2],
    outlier.to        = outliers_span_details$to,        #regarima_spec$span$d0[2],
    outlier.first     = outliers_span_details$first,     #ifelse( !is.na(regarima_spec$span$d1[2]) || !is.na(regarima_spec$span$d0[2]) || !is.na(regarima_spec$span$n1[2]),NA_integer_,regarima_spec$span$n0[2]),
    outlier.last      = outliers_span_details$last,      #ifelse( !is.na(regarima_spec$span$d1[2]) || !is.na(regarima_spec$span$d0[2]), NA_integer_, regarima_spec$span$n1[2]),
    outlier.exclFirst = outliers_span_details$exclFirst, #ifelse((!is.na(regarima_spec$span$d1[2]) || !is.na(regarima_spec$span$d0[2]) || !is.na(regarima_spec$span$n0[2]) || !is.na(regarima_spec$span$n1[2])) && !(regarima_spec$span$type[2] %in% c("From", "To", "Between")),  NA_integer_, regarima_spec$span$n0[2]),
    outlier.exclLast  = outliers_span_details$exclLast,  #ifelse((!is.na(regarima_spec$span$d1[2]) || !is.na(regarima_spec$span$d0[2]) || !is.na(regarima_spec$span$n0[2]) || !is.na(regarima_spec$span$n1[2])) && !(regarima_spec$span$type[2] %in% c("From", "To", "Between")),  NA_integer_, regarima_spec$span$n1[2]),
    outlier.ao       = regarima_spec$outliers$ao,
    outlier.tc       = regarima_spec$outliers$tc,
    outlier.ls       = regarima_spec$outliers$ls,
    outlier.so       = regarima_spec$outliers$so,
    outlier.usedefcv = regarima_spec$outliers$usedefcv,
    outlier.cv       = regarima_spec$outliers$cv,
    outlier.eml      = regarima_spec$outliers$eml,
    outlier.tcrate   = regarima_spec$outliers$tcrate,
    automdl.enabled  = regarima_spec$arima$specification$enabled,
    automdl.acceptdefault = regarima_spec$arima$specification$automdl.acceptdefault,
    automdl.cancel   = regarima_spec$arima$specification$automdl.cancel,
    automdl.ub1      = regarima_spec$arima$specification$automdl.ub1,
    automdl.ub2      = regarima_spec$arima$specification$automdl.ub2,
    automdl.armalimit= regarima_spec$arima$specification$automdl.armalimit,
    automdl.reducecv = regarima_spec$arima$specification$automdl.reducecv,
    automdl.ljungboxlimit = regarima_spec$arima$specification$automdl.ljungboxlimit,
    automdl.compare  = regarima_spec$arima$specification$compare,
    arima.mu = regarima_spec$arima$specification$arima.mu,
    arima.p  = regarima_spec$arima$specification$arima.p,
    arima.d  = regarima_spec$arima$specification$arima.d,
    arima.q  = regarima_spec$arima$specification$arima.q,
    arima.bp = regarima_spec$arima$specification$arima.bp,
    arima.bd = regarima_spec$arima$specification$arima.bd,
    arima.bq = regarima_spec$arima$specification$arima.bq,
    arima.coef        = get_arima_coef_info(regarima_spec)$value, #ifelse(is.na(regarima_spec$arima$coefficients), NA, regarima_spec$arima$coefficients$Value),
    arima.coefEnabled = regarima_spec$arima$specification$arima.coef,
    arima.coefType    = get_arima_coef_info(regarima_spec)$type, #ifelse(regarima_spec$arima$specification$arima.coef, regarima_spec$arima$coefficients$Type, NA), # controllare
    fcst.horizon      = regarima_spec$forecast$horizon,
    seats.predictionLength = seats_spec$seats.predictionLength,
    seats.approx           = seats_spec$seats.approx,
    seats.trendBoundary    = seats_spec$seats.trendBoundary,
    seats.seasdBoundary    = seats_spec$seats.seasdBoundary,
    seats.seasdBoundary1   = seats_spec$seats.seasdBoundary1,
    seats.seasTol          = seats_spec$seats.seasTol,
    seats.maBoundary       = seats_spec$seats.maBoundary,
    seats.method           = seats_spec$seats.method,
    ramps                  = ramps,
    intervention_variables = intervention_variables,
    # leap_yearCoef         = NA,
    easterCoef            = easterCoef
    #rampsCoef             = rampsCoef,
    #intervention_variablesCoef = NA
  )
  #browser()
  return(extended_tramoseats_spec)
}



read_spec_list_from_json_file <- function(file_name, spec_format="Extended_tramoseats_spec") {

  require(rjson)
  #browser()

  # print("here\n")
  # print(file_name)

  json_text <- readLines(file_name)

  # Comments removal pt1 of 2
  # How to do it? at this point, remove only the text between // (included) at the end of the line
  # Removes single rows comments (the ones that start with //)
  json_text <- gsub("//.*$", "", json_text)


  # Merge the rows in a single string
  json_string <- paste(json_text, collapse = "")

  # Comments removal pt2 of 2
  # How to do it? at this point, remove only /*...*/ multiline comments, considreing that all the lines have been merged into one
  # Removes multiline comments (the ones which starts with /* and ends with */)
  # json_string <- gsub("/\\*.*?\\*/", "", json_string, fixed = TRUE)
  json_string <- gsub("/\\*(.|\\n)*?\\*/", "", json_string)


  # Parsing of JSON array
  json_array <- fromJSON(json_string)

  json_list <- list()
  i=1
  for (json in json_array)
  {
    json <- NA_not_as_char(json)

    #browser()
    if(spec_format=="Extended_tramoseats_spec")
    {
      json  <- do.call(Extended_tramoseats_spec, json)
      series_name <- json@series_name
    } else {series_name <- json$series_name}


    if(!is.null(series_name)){
      json_list[[series_name]]=json
    } else{
      json_list[[as.character(i)]]=json
      i=i+1
    }

  }

  return(json_list)

}


to_tramoseats_spec_args<-function(extended_tramoseats_spec, data_reader_ext_reg)
{
  # TODO: remove also ramps, intervention_variables, rampsCoef, intervention_variablesCoef and easterCoef that are not supported in R

  # Extract the elements which are in Extended_tramoseats_spec and not in tramoseats_spec
  userdef.varFromFile          <- extended_tramoseats_spec$userdef.varFromFile
  userdef.varFromFile.infoList <- extended_tramoseats_spec$userdef.varFromFile.infoList
  series_name                  <- extended_tramoseats_spec$series_name
  frequency                    <- extended_tramoseats_spec$frequency
  method                       <- extended_tramoseats_spec$method

  # Remove the elements present in Extended_tramoseats_spec and not in tramoseats_spec
  extended_tramoseats_spec <- extended_tramoseats_spec[ ! names(extended_tramoseats_spec) %in% c("series_name") ] # è già memorizzato in ts_name; lo tolgo
  extended_tramoseats_spec <- extended_tramoseats_spec[ ! names(extended_tramoseats_spec) %in% c("frequency") ]
  extended_tramoseats_spec <- extended_tramoseats_spec[ ! names(extended_tramoseats_spec) %in% c("method") ]
  extended_tramoseats_spec <- extended_tramoseats_spec[ ! names(extended_tramoseats_spec) %in% c("userdef.varFromFile") ]
  extended_tramoseats_spec <- extended_tramoseats_spec[ ! names(extended_tramoseats_spec) %in% c("userdef.varFromFile.infoList") ]


  # If "estimate.first", "estimate.last", "outlier.first", "outlier.last" are "NA" (string), remove them
  suspectedNA <- c("estimate.first", "estimate.last", "outlier.first", "outlier.last")

  for(field in suspectedNA)
  {
    if(!is.null(extended_tramoseats_spec[[field]]) && is.na(extended_tramoseats_spec[[field]]))
    {
      extended_tramoseats_spec <- extended_tramoseats_spec[ ! names(extended_tramoseats_spec) %in% c(field) ]
    }
  }


  #browser()

  # Use the elements extracted from Extended_tramoseats_spec to prepare the external regressors
  if(!(is.null(userdef.varFromFile) || userdef.varFromFile == FALSE)){
    if(!is.null(userdef.varFromFile.infoList)){

      # the  ext_regr_data_reader wants a named list of info as an input
      info_list <- list()
      info_list[[series_name]]  <- userdef.varFromFile.infoList

      vars_mts                  <- data_reader_ext_reg@read_ext_reg_data(info_list, series_name, frequency) #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      extended_tramoseats_spec[["usrdef.var"]] <- vars_mts
    }
  }

  tramoseats_spec <- extended_tramoseats_spec
  return(tramoseats_spec)

}



to_SA_spec<-function(extended_tramoseats_spec)
{
  tramoseats_spec_args <- to_tramoseats_spec_args(extended_tramoseats_spec)
  tramoseats_spec      <- do.call(RJDemetra::tramoseats_spec, tramoseats_spec_args)
  return(tramoseats_spec)
}



get_ramps_from_java<- function(series)
{
  #browser()

  ramps_ret <- list()

  regression<-series$spec$getRegression()
  core<-regression$getCore()
  core_regression<-core$getRegression()
  ramps_count<-core_regression$getRampsCount() # for loop over this


  i<-0
  while(i < ramps_count)
  {
    ramp_i       <- core_regression$getRamp(as.integer(i))
    begin_ramp_i <- ramp_i$getStart()$toString()
    end_ramp_i   <- ramp_i$getEnd()$toString()
    name_ramp_i  <- ramp_i$getName()
    ramp_i_ret   <- list("start"=begin_ramp_i, "end"=end_ramp_i, name=name_ramp_i)

    ramps_ret[[length(ramps_ret) + 1]] <- ramp_i_ret

    i<-i+1
  }
  return(ramps_ret)


}


get_intervention_variables_from_java<- function(series)
{
  #browser()

  ivs_ret <- list()

  regression<-series$spec$getRegression()

  core<-regression$getCore()
  core_regression<-core$getRegression()
  ivs_count<-core_regression$getInterventionVariablesCount() # for loop over this

  if(ivs_count>0)
  {
    message("NOTE: Your project seems to contain INTERVENTION VARIABLES: It is
      not possible to FIX INTERVENTION VARIABLES coefficients,
      if you want to do it, convert them into UserDefined Variables, if you don't
      need to do it, everything is ok")
  }

  i<-0
  while(i < ivs_count)
  {
    iv_i         <- core_regression$getInterventionVariable(as.integer(i))

    # begin_ramp_i <- ramp_i$getStart()$toString()
    # end_ramp_i   <- ramp_i$getEnd()$toString()
    # name_ramp_i  <- ramp_i$getName()
    # ramp_i_ret   <- list("start"=begin_ramp_i, "end"=end_ramp_i, name=name_ramp_i)
    #
    # ramps_ret[[length(ramps_ret) + 1]] <- ramp_i_ret

    i<-i+1
  }
  return(ivs_ret)


}

