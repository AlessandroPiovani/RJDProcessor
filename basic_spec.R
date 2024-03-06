get_basic_spec <- function(spec)
{
  if(spec=="RSA0")
  {
    RSA0 <- list(
      series_name = NA,
      spec = "RSA0",
      preliminary.check = TRUE,
      estimate.from = NA_character_,
      estimate.to = NA_character_,
      estimate.first = NA_integer_,
      estimate.last = NA_integer_,
      estimate.exclFirst = 0,
      estimate.exclLast  = 0,
      estimate.tol = 1e-07,
      estimate.eml = TRUE,
      estimate.urfinal = 0.96,
      transform.function = "None",
      transform.fct = 0.95,
      usrdef.outliersEnabled = FALSE,
      usrdef.outliersType = NA,
      usrdef.outliersDate = NA,
      usrdef.outliersCoef = NA,
      userdef.varFromFile = FALSE,
      userdef.varFromFile.infoList = NULL,
      usrdef.varEnabled = FALSE,
      usrdef.var = NA,
      usrdef.varType = NA,
      usrdef.varCoef = NA,
      tradingdays.mauto = "Unused",
      tradingdays.pftd = 0.01,
      tradingdays.option = "None",
      tradingdays.leapyear = FALSE,
      tradingdays.stocktd = 0,
      tradingdays.test = "None",
      easter.type = "Unused",
      easter.julian = FALSE,
      easter.duration = 6,
      easter.test = FALSE,
      outlier.enabled = FALSE,
      outlier.from = NA_character_,
      outlier.to = NA_character_,
      outlier.first = NA_integer_,
      outlier.last = NA_integer_,
      outlier.exclFirst = 0,
      outlier.exclLast = 0,
      outlier.ao = FALSE,
      outlier.tc = FALSE,
      outlier.ls = FALSE,
      outlier.so = FALSE,
      outlier.usedefcv = TRUE,
      outlier.cv = 3.5,
      outlier.eml = FALSE,
      outlier.tcrate = 0.7,
      automdl.enabled = FALSE,
      automdl.acceptdefault = FALSE,
      automdl.cancel = 0.05,
      automdl.ub1 = 0.97,
      automdl.ub2 = 0.91,
      automdl.armalimit = 1,
      automdl.reducecv = 0.12,
      automdl.ljungboxlimit = 0.95,
      automdl.compare = FALSE,
      arima.mu = TRUE,
      arima.p = 0,
      arima.d = 1,
      arima.q = 1,
      arima.bp = 0,
      arima.bd = 1,
      arima.bq = 1,
      arima.coefEnabled = FALSE,
      arima.coef = NA,
      arima.coefType = NA,
      fcst.horizon = -2,
      seats.predictionLength = -1,
      seats.approx = "Legacy",
      seats.trendBoundary = 0.5,
      seats.seasdBoundary = 0.8,
      seats.seasdBoundary1 = 0.8,
      seats.seasTol = 2,
      seats.maBoundary = 0.95,
      seats.method = "Burman"
    )
    return(RSA0)
  }else if(spec == "RSA1")
  {
    # TODO
    paste("not implemented yet")
  }
  
  #...
  
  else
  {  
    return(NA)
  }  
  # TODO: implement all the basic specs
}