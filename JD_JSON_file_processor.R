
# Carica il pacchetto RJDemetra
#install.packages("RJDemetra")
require(RJDemetra)
#install.packages("rjson")
require(rjson)

source("utility_functions.R")
source("Extended_tramoseats_spec.R")
source("basic_spec.R")
source("JD_JSON.R")

#source("Data_reader") #substitute with a specific Data reader
source("Data_reader_csv_istat_format.R") #substitute with custom data_reader if this format does not fit your needs
source("Data_reader_ext_reg_tsplus.R")   #substitute with custom data_reader if this format does not fit your needs





JD_JSON_file_processor <- function(input_provider, ext_var_provider, spec_file_name, output_workspace_dir=NA, series_to_proc_names=NA)#, data_reader_ext_reg = NA)
{
    #browser()
    wk <- JD_JSON_to_materialized_workspace(workspace_dir=output_workspace_dir, JSON_file = JSON_file, input_provider = input_provider, ext_reg_provider= ext_var_provider, series_to_proc_names = series_to_proc_names)
    
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
    
    return(model)
}


