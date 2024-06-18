require(RJDemetra)
require(rjson)

source("utility_functions.R")
source("JD_JSON.R")


JD_JSON_file_processor <- function(input_data_reader, ext_reg_data_reader, spec_file_name, output_workspace_dir=NA, series_to_proc_names=NA, java_processing=TRUE)
{
    wk <- JD_JSON_to_materialized_workspace(workspace_dir=output_workspace_dir, JSON_file = JSON_file, input_data_reader = input_data_reader, ext_reg_data_reader= ext_reg_data_reader, series_to_proc_names = series_to_proc_names)
    
    
    if(java_processing==FALSE)
    {
      model=get_model(wk)
    } else
    {
      j_model <- get_jmodel(wk)
      model   <- get_r_model_from_j_model(j_model)  
    } 
        
    
    zz <- file("elaborazione.out", open="wt")
    sink(zz, type)
    
    print(model)
    
    sink(file=NULL)
    
    i=1
    nomi_serie = names(model[[1]])
    
    pdf(file="plots.pdf")
    
    for(serie in model[[1]])
    {
      #print(nomi_serie[i])
      plot.new()
      text(x=.5, y=.5, nomi_serie[i], cex=2)  # first 2 numbers are xy-coordinates within [0, 1]
      plot(serie, type_chart = "sa-trend")
      plot(serie$decomposition)
      
      i=i+1
      
    }  
    
    dev.off()
    close.connection(zz)
    
    #return(model)
    return(wk)
}





get_r_model_from_j_model <- function(j_model)
{
  #browser()
  model=list()
  k=1
  for(sa_name in names(j_model))
  { 
    sa <- j_model[[sa_name]]
    model[[sa_name]] <- sa
    for (j_time_series_name in names(sa)) {
      #browser()
      #print(j_time_series_name)
      r_time_series    <- jSA2R(sa[[j_time_series_name]]) #error with FATEXP_14
      model[[sa_name]][[j_time_series_name]] <- r_time_series
    }
  }
  return(model)
}



compare_sa_ts <- function(new_r_model=NA, new_model_workspace, old_model_workspace, materialized_ws_new=FALSE, materialized_ws_old=TRUE, java_processing_old_model=TRUE)
{
  #browser()
  if(!is.na(new_r_model))
  {
    model_new <- new_r_model
  }
  else
  {
    if(materialized_ws_new == FALSE)
    {
      compute(new_model_workspace)
      j_model_new <- get_jmodel(new_model_workspace)
    } else
    {
      j_ws_new <- RJDemetra::load_workspace(new_model_workspace)
      compute(j_ws_new)
    }  
    model_new <- get_r_model_from_j_model(j_model_new) 
  }
 
  if(materialized_ws_old == FALSE)
  {
    compute(j_ws_old)
    j_ws_old <- get_jmodel(old_model_workspace)
  } else
  {
    j_ws_old <- RJDemetra::load_workspace(old_model_workspace)
    compute(j_ws_old)
  }
  
  if(java_processing_old_model==TRUE)
  {
    model_old <- get_jmodel(j_ws_old)
    model_old <- get_r_model_from_j_model(model_old)  
  } else
  {
    model_old <- get_model(j_ws_old)
  } 
  
  
  

  pdf(file="comparisons.pdf")
  
  i=1
  series_names = names(model_new[[1]])
  
  for(series in model_new[[1]])
  {
    new_model_ts   <- series$final$series[,"sa"]
    new_model_name <- series_names[i]
    
    #browser()
    old_model_ts <- model_old[[1]][[new_model_name]]$final$series[,"sa"]
    plot(new_model_ts,  col = "blue", lty = 1, ylab = "Values", xlab = "Time", main = new_model_name)
    if(!is.null(model_old[[1]][[new_model_name]]) && !all(is.na(model_old[[1]][[new_model_name]])))
    {
      lines(old_model_ts, col = "red",  lty = 2)
      legend("topright", legend = c("new model", "old model"), col = c("blue", "red"), lty = c(1, 2))
    } else 
    {
      legend("topright", legend = c("new model"), col = c("blue"), lty = c(1))
    }

    i=i+1
    
  }
  dev.off()
  closeAllConnections()

}
