source("basic_spec.R")
require(RJDemetra)


# Dichiarazione della classe
setClass(
  "Extended_tramoseats_spec",
  slots = list(
    series_name             = "character",    # ISTAT custom field
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
    seats.method            = "ANY"
    
    #tramoseats_spec         = "ANY"
  )
)


# Costruttore

  #costruttore initialize per la classe
setMethod("initialize", "Extended_tramoseats_spec",
          function(.Object, series_name, spec = "RSA0", preliminary.check = NA,
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
                   seats.maBoundary = NA_integer_, seats.method = NA) {
            
            
            # Convert possible numeric arguments to integer if they are compatible
            integer_args_to_check <- c( "estimate.first", "estimate.last", "estimate.exclFirst", "estimate.exclLast", "tradingdays.stocktd",
                                "easter.duration", "outlier.first", "outlier.last", "outlier.exclFirst", "outlier.exclLast", 
                                "arima.p", "arima.d", "arima.q", "arima.bp", "arima.bd", "arima.bq",
                                "seats.predictionLength")
            
            numeric_args_with_NA_integer <- c("estimate.tol", "transform.fct", "outlier.cv", "outlier.tcrate", "automdl.cancel", "automdl.ub1", "automdl.ub2", "automdl.armalimit", "automdl.reducecv", "automdl.ljungboxlimit", "fcst.horizon", "seats.trendBoundary", "seats.seasdBoundary", "seats.seasdBoundary1", "seats.seasTol", "seats.maBoundary")
            
            for (arg_name in integer_args_to_check) {
              
              # if(arg_name=="estimate.exclFirst"){
              #    browser()
              # }
              if (!is.na(eval(parse(text = arg_name))) && is.numeric(eval(parse(text = arg_name))) && all(eval(parse(text = arg_name)) %% 1 == 0)) {
                assign(arg_name, as.integer(eval(parse(text = arg_name))))
              } else if (is.na(eval(parse(text = arg_name))))
              {
                assign(arg_name, NA_integer_)
              }  
            } 
            
            for (arg_name in numeric_args_with_NA_integer) {
              if (is.na(eval(parse(text = arg_name))))
              {
                assign(arg_name, NA_integer_)
              }  
            }   
            
            
 
            
            .Object@series_name <- series_name
            .Object@spec        <- spec
            
            # All the following rows are replaced by the dynamic evaluation below them
            # This is done because if an argument is not specified, we want the default given by spec and not NA
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
            
            
            attributes <- c("preliminary.check", "estimate.from", "estimate.to", "estimate.first", "estimate.last", "estimate.exclFirst", "estimate.exclLast", "estimate.tol", "estimate.eml", "estimate.urfinal", "transform.function", "transform.fct", "usrdef.outliersEnabled", "usrdef.outliersType", "usrdef.outliersDate", "usrdef.outliersCoef", "userdef.varFromFile", "userdef.varFromFile.infoList", "usrdef.varEnabled", "usrdef.var", "usrdef.varType", "usrdef.varCoef", "tradingdays.mauto", "tradingdays.pftd", "tradingdays.option", "tradingdays.leapyear", "tradingdays.stocktd", "tradingdays.test", "easter.type", "easter.julian", "easter.duration", "easter.test", "outlier.enabled", "outlier.from", "outlier.to", "outlier.first", "outlier.last", "outlier.exclFirst", "outlier.exclLast", "outlier.ao", "outlier.tc", "outlier.ls", "outlier.so", "outlier.usedefcv", "outlier.cv", "outlier.eml", "outlier.tcrate", "automdl.enabled", "automdl.acceptdefault", "automdl.cancel", "automdl.ub1", "automdl.ub2", "automdl.armalimit", "automdl.reducecv", "automdl.ljungboxlimit", "automdl.compare", "arima.mu", "arima.p", "arima.d", "arima.q", "arima.bp", "arima.bd", "arima.bq", "arima.coefEnabled", "arima.coef", "arima.coefType", "fcst.horizon", "seats.predictionLength", "seats.approx", "seats.trendBoundary", "seats.seasdBoundary", "seats.seasdBoundary1", "seats.seasTol", "seats.maBoundary", "seats.method")
            basic_spec <- get_basic_spec("RSA0")

            for (attr in attributes) {
              if (!all(is.na(eval(parse(text = attr))))) {
                slot(.Object, attr) <- eval(parse(text = attr))
              }else{
                if(attr %in% integer_args_to_check){ 
                  basic_spec[[attr]]<-as.integer(basic_spec[[attr]])  
                }
                slot(.Object, attr) <- basic_spec[[attr]]
              }
            }
            
            
            
            
            
            return(.Object)
          })




  # Costruttore R-like: 
        # Funzione di aiuto per creare un oggetto Extended_tramoseats
Extended_tramoseats_spec_helper <- function(series_name = NULL, spec = NULL, preliminary.check = NA,
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
                                        seats.method = NA) {
  
  new("Extended_tramoseats_spec", series_name = series_name, spec = spec, preliminary.check = preliminary.check,
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
      seats.maBoundary = seats.maBoundary, seats.method = seats.method)
}

        # Definizione dell'alias di funzione (costruttore R-like)
Extended_tramoseats_spec <- Extended_tramoseats_spec_helper







# Definizione del metodo to_JD_JSON #se lo definisco così, non riesco a chiamarlo in from_reduced_to_full_JD_JSON_file di JD_JSON.r, perciò faccio definizione normale
# basic_spec di default "RSA0", funzionalità di costruzione file su basic_spec diverse non ancora implementata
setGeneric("to_JD_JSON", function(object, indent = FALSE, diff = TRUE, basic_spec="RSA0") standardGeneric("to_JD_JSON"))
setMethod("to_JD_JSON", "Extended_tramoseats_spec", function(object, indent = FALSE, diff = TRUE, basic_spec="RSA0") {
#to_JD_JSON <- function(object, indent = FALSE, diff = TRUE)
#{  
  require(rjson)
  
  if(diff == TRUE)
  {  
    #browser()
    RSA0   <- from_SA_spec(SA_spec = tramoseats_spec("RSA0"), userdef.varFromFile = FALSE)
    object <- difference_objects_preserving_name_and_spec(object, basic = RSA0)
  }
  else
  {
    object <- to_named_list(object)
  }
  
  # Converti l'oggetto in JSON utilizzando rjson
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
    json_spec <- gsub("\\}(?!\\])", "\n}", json_spec, perl = TRUE)
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




# Definizione del metodo from_JD_JSON
setGeneric("from_JD_JSON", function(object, json) standardGeneric("from_JD_JSON"))
setMethod ("from_JD_JSON", "Extended_tramoseats_spec", function(object, json) {
  require(rjson)
  json_list <- fromJSON(json)
  
  # Inizializza gli argomenti per il costruttore
  args <- list(
    series_name = json_list$series_name,
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
    seats.method = json_list$seats.method
  )
  
  # Rimuovi gli argomenti non specificati
  args <- args[sapply(args, Negate(is.null))]
  
  # Costruisci un nuovo oggetto Extended_tramoseats_spec
  do.call("Extended_tramoseats_spec", args)

})

















to_named_list <- function(object) {
  # Ottieni i nomi degli slot dell'oggetto S4
  slot_names <- slotNames(class(object))
  
  # Inizializza una named list vuota
  result_list <- list()
  
  # Aggiungi i valori degli slot alla named list utilizzando slot()
  for (slot_name in slot_names) {
    result_list[[slot_name]] <- slot(object, slot_name)
  }
  
  return(result_list)
}



extended_tramoseats_spec_list <-  function(workspace, regr_directory=NA, ...)
{
  compute(workspace)
  
  #browser()
  
  jmodel          <- RJDemetra::get_jmodel(workspace, progress_bar = TRUE) # to retrieve external regressors by name
  m               <- get_model(workspace, progress_bar = TRUE)
  
  print("Models loaded")
  
  all_jmodel_vars <- getUserDefinedTdVariables_info(jmodel)

  extended_tramoseats_spec_list <- list()
  for (series_name in names(m[[1]]))
  {
    series        <-  m[[1]][[series_name]]
    basic_spec    <- get_jspec(jmodel[[1]][[series_name]])$toString()  
    
    #browser()
    spec <- from_SA_spec(series, series_name = series_name, basic_spec="RSA0", regr_directory = regr_directory, all_jmodel_vars=all_jmodel_vars)
    spec <- list(spec)
    extended_tramoseats_spec_list <- append(extended_tramoseats_spec_list ,spec)
  }
  
  gc() # to clean memory after the use of rJava
  
  return(extended_tramoseats_spec_list)
  
}  





from_SA_spec <- function(SA_spec, series_name = NA_character_, basic_spec="RSA0", regr_directory = getwd(), all_jmodel_vars=NULL, userdef.varFromFile=TRUE)
{  
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
      
      if(!is.null(all_jmodel_vars))
      {
        if(is.na(series_name))
        {
          warning("Impossible to read external variables without specifying series name! The procedure ends without considering extrernal variables", call=TRUE)
        }else
        { 
          vars_mts          <- getUserDef_var_as_mts(all_jmodel_vars, regr_directory, series_name) # Read as example but not used now 
          user_def_var_info <- get_user_def_var_info(regarima_spec) # si può prendere anche dal workspace?
          usrdef.varType <- user_def_var_info$type
          usrdef.varCoef <- user_def_var_info$coef
          
          #browser()
          userdef.varFromFile.infoList = all_jmodel_vars[[series_name]] 
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
      usrdef.varCoef <- NA  #get_user_def_var_info$Coef ?
      userdef.varFromFile.infoList <- NULL
    } 
    
    
    extended_tramoseats_spec <- Extended_tramoseats_spec(
      spec               = ifelse(basic_spec=="TS", "RSA0", basic_spec), 
      series_name        = series_name, # ISTAT custom field
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
      usrdef.var             = ifelse(userdef.varFromFile==TRUE, NA,vars_mts), # !!!
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
      seats.method           = seats_spec$seats.method 
    )
    
    return(extended_tramoseats_spec)
}

# if spec_format=="Extended_tramoseats_spec" the function returns a list of "Extended_tramoseats_object", if spec_format=="list", the elements of the list are named lists with the same names of the objects elements.
# a list of "Extended_tramoseats_object"s is heavier in terms of memory than a list of lists
read_spec_list_from_json_file <- function(file_name, spec_format="Extended_tramoseats_spec") {
  
  # Carica la libreria rjson
  library(rjson)
  ##browser()
  
  # Leggi il contenuto del file specifications.txt
  json_text <- readLines(file_name)
  
  # Rimuovi i commenti pt1 di 2
  # Come fare? a questo punto rimuovere solo il testo compreso fra // (inclusi) a fine linea
  # Rimuovi i commenti singola riga (quelli che iniziano con //)
  json_text <- gsub("//.*$", "", json_text)
  
  
  # Unisci le righe in una singola stringa
  json_string <- paste(json_text, collapse = "")
  
  # Rimuovi i commenti pt2
  # Come fare? a questo punto rimuovere solo /*...*/ commenti multilinea, visto che le linee sono state tutte unite in una sola
  # Rimuovi i commenti multilinea (quelli che iniziano con /* e terminano con */)
  # json_string <- gsub("/\\*.*?\\*/", "", json_string, fixed = TRUE)
  json_string <- gsub("/\\*(.|\\n)*?\\*/", "", json_string)
  
  
  # Parsa il JSON array
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



to_tramoseats_spec_args<-function(extended_tramoseats_spec, regr_directory)
{
  
  # Estrai gli elementi che ci sono in Extended_tramoseats_spec e non in tramoseats_spec
  userdef.varFromFile          <- extended_tramoseats_spec$userdef.varFromFile
  userdef.varFromFile.infoList <- extended_tramoseats_spec$userdef.varFromFile.infoList
  
  # Rimuovi gli elementi che ci sono in Extended_tramoseats_spec e non in tramoseats_spec
  extended_tramoseats_spec <- extended_tramoseats_spec[ ! names(extended_tramoseats_spec) %in% c("series_name") ] # è già memorizzato in ts_name; lo tolgo 
  extended_tramoseats_spec <- extended_tramoseats_spec[ ! names(extended_tramoseats_spec) %in% c("userdef.varFromFile") ]
  extended_tramoseats_spec <- extended_tramoseats_spec[ ! names(extended_tramoseats_spec) %in% c("userdef.varFromFile.infoList") ]
  
  #browser()
  
  #Usa gli elementi estratti da Extended_tramoseats_spec per preparare i regressori esterni
  if(!(is.null(userdef.varFromFile) || userdef.varFromFile == FALSE)){
    if(!is.null(userdef.varFromFile.infoList)){
      mts <- getUserDef_var_as_mts2(userdef.varFromFile.infoList, regr_directory)  
      extended_tramoseats_spec[["usrdef.var"]] <- mts
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










