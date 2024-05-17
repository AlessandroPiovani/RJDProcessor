require(RJDemetra)
require(rjson)

source("utility_functions.R")
source("JD_JSON.R")


JD_JSON_file_processor <- function(input_data_provider, ext_reg_provider, spec_file_name, output_workspace_dir=NA, series_to_proc_names=NA)
{
    #browser()
    wk <- JD_JSON_to_materialized_workspace(workspace_dir=output_workspace_dir, JSON_file = JSON_file, input_data_provider = input_data_provider, ext_reg_provider= ext_reg_provider, series_to_proc_names = series_to_proc_names)
    
    model=get_model(wk)
    
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


