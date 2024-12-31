require(RJDemetra)
require(rjson)
#source("utility_functions.R")
#source("JD_JSON.R")


#' Process a JD_JSON file
#'
#' This function processes a JSON file with JD_JSON fields and returns a virtual
#' workspace
#'
#' @param input_data_reader A specific Data_reader object (CSV, XLSX, ...) to read
#'                          the input
#' @param ext_reg_data_reader A specific Data_reader_ext_reg object (CSV, XLSX, ...)
#'                            to read the external regrssors
#' @param spec_file_name Name of the file (with path, if desired) containing the
#'                       JD_JSON to be processed (e.g. "specification_new.txt")
#' @param output_workspace_dir -optional- Name of the directory that will contain
#'                             the output workspace. Default=NA stores the workspace
#'                             in a directory called "output_workspace_container"
#' @param series_to_proc_names -optional- vector of names of time series to be
#'                             processed (e.g. c('VATASA','VATPIA')) .
#'                             Default=NA: all the series are processed
#' @param java_processing -optional- Default=TRUE. Use only Java API (faster) for
#'                        the internal computation
#' @return a virtual workspace, already processed
#' @examples
#' require(RJDemetra)
#' original_directory <- getwd()
#' extdata_directory  <- system.file("extdata", package = "RJDProcessor")
#' setwd(extdata_directory)
#' spec_file_name <- "specifications_to_proc.txt"
#' input_workspace_directory <- "WorkspaceTUR-container/workspace-TUR.xml"
#' input_data_file_name      <- "CSV-TUR/grezzi_trim_TUR.csv"
#' regr_directory            <- "CSV-TUR/regr"
#' diff <- TRUE # Reduced JSON if diff=TRUE, Full JSON format otherwise
#'
#'############################# Operational flow ###############################
#'
#' input_data_reader <- Data_reader_csv(input_source = input_data_file_name)
#' ext_reg_input_data_reader <- Data_reader_ext_reg_csv(regr_directory)
#' JD_JSON_from_materialized_workspace(input_workspace_directory,
#'          ext_reg_input_data_reader, JSON_file_name = spec_file_name,
#'          diff=TRUE, java_processing=FALSE)
#' series_to_proc_names <- NA #c("FATEXP_13", "C_DEFL") # NA to process all
#' virtual_workspace <- JD_JSON_file_processor(input_data_reader = input_data_reader,
#'          ext_reg_data_reader = ext_reg_input_data_reader,
#'          spec_file_name = spec_file_name,
#'          output_workspace_dir = "output_workspace_container",
#'          series_to_proc_names = series_to_proc_names,
#'          java_processing = FALSE)
#' # set java_processor=TRUE to speed-up the operations, but it does not work with
#' # workspaces readed by sa-ext plugin
#' m <- get_model(virtual_workspace) #get directly the R model (slower)
#' from_reduced_to_full_JD_JSON_file(spec_file_name)
#' compare_sa_ts(new_model_workspace = virtual_workspace,
#'               old_model_workspace = input_workspace_directory ,
#'               materialized_ws_new=FALSE, materialized_ws_old=TRUE,
#'               java_processing_old_model=FALSE)
#' setwd(original_directory)
#' @export
JD_JSON_file_processor <- function(input_data_reader, ext_reg_data_reader, spec_file_name, output_workspace_dir=NA, series_to_proc_names=NA, java_processing=TRUE)
{
  require(RJDemetra)

  #browser()

  wk <- JD_JSON_to_materialized_workspace(workspace_dir=output_workspace_dir, JSON_file = spec_file_name, input_data_reader = input_data_reader, ext_reg_data_reader= ext_reg_data_reader, series_to_proc_names = series_to_proc_names)


  if(java_processing==FALSE)
  {
    model=get_model(wk)
  } else
  {
    j_model <- get_jmodel(wk)
    # model   <- get_r_model_from_j_model(j_model)
    model   <- j_model
  }
  #browser()

  if(java_processing==FALSE)
  {
    zz <- file("processing_results.out", open="wt")
    sink(zz, type)
    print(model)
    sink(file=NULL)

    name_series = names(model[[1]])

    i=1
    pdf(file="plots.pdf")

    for(serie in model[[1]])
    {
      #browser()
      #print(name_series[i])
      plot.new()
      text(x=.5, y=.5, name_series[i], cex=2)  # first 2 numbers are xy-coordinates within [0, 1]
      plot(serie, type_chart = "sa-trend")

      serie_plot <- serie

      # if decomposition is not feasible some columns are not plottable (all NA): set values to 0
      if (all(is.na(serie_plot$decomposition[["linearized"]][, "s_lin"]))) {
        serie_plot$decomposition[["linearized"]][, "s_lin"] <- 0
      }
      if (all(is.na(serie_plot$decomposition[["components"]][, "s_cmp"]))) {
        serie_plot$decomposition[["components"]][, "s_cmp"] <- 0
      }

      plot(serie_plot$decomposition)

      i=i+1
    }

    dev.off()
    close.connection(zz)
  }
  else # Java Processing == TRUE
  {
    print("No processing results file with Java processing yet (waiting for rjdmarkdown package to be fixed)")
  }


  return(wk)
}



#' Process a JD_JSON file
#'
#' This function processes a JSON file with JD_JSON fields and returns a virtual
#' workspace
#'
#' @param ext_reg_input_data_reader A specific Data_reader_ext_reg object
#'                                  (CSV, XLSX, ...) to read the external
#'                                  regressors. Default=NA.
#' @param original_ws_xml xml file (also with path) of the workspace to be
#'                        transformed into a fully R workspace
#' @param new_r_ws_folder Name of the directory that will contain
#'                        the output, fully R, workspace.
#' @return a virtual workspace, already processed
#' @examples
#' require(RJDemetra)
#' original_directory <- getwd()
#' extdata_directory  <- system.file("extdata", package = "RJDProcessor")
#' setwd(extdata_directory)
#' original_ws_xml <- "WorkspaceTUR-container/workspace-TUR.xml"
#' new_r_ws_folder <- "output_fully_R_ws"
#' regr_directory  <- "SITIC-TUR/regr"
#' ext_reg_input_data_reader <- Data_reader_ext_reg_tsplus(regr_directory)
#' produce_fully_R_workspace(ext_reg_input_data_reader, original_ws_xml, new_r_ws_folder)
#'
#'
#' extdata_directory  <- system.file("extdata", package = "RJDProcessor")
#' setwd(extdata_directory)
#' original_ws_xml <- "WorkspaceFAS-standard-container//FAS.xml"
#' new_r_ws_folder <- "output_fully_R_ws"
#' regr_directory  <- "CSV-FAS//regr"
#' ext_reg_input_data_reader <- Data_reader_ext_reg_csv(regr_directory)
#' produce_fully_R_workspace(ext_reg_input_data_reader, original_ws_xml, new_r_ws_folder)
#' setwd(original_directory)
#'
#' @export
produce_fully_R_workspace <- function(ext_reg_input_data_reader=NA, original_ws_xml, new_r_ws_folder)
{
  require(RJDemetra)

  # original_ws_xml <-input_workspace
  input_data_reader <- Data_reader_workspace(original_ws_xml)

  #browser()
  JD_JSON_from_materialized_workspace(original_ws_xml, ext_reg_input_data_reader, JSON_file_name = "spec_temp.txt", diff=TRUE, java_processing=FALSE)
  ws<-JD_JSON_to_materialized_workspace(input_data_reader = input_data_reader, ext_reg_data_reader = ext_reg_input_data_reader, JSON_file = "spec_temp.txt", workspace_dir = new_r_ws_folder)
  file.remove("spec_temp.txt")
  return(ws)
}


#' Get an R list with model information from java model
#'
#' This function gets an R list with model information from java model
#'
#' @param j_model a Java model obtained from RJDemetra
#' @return a list containing all the models specifications
#' @examples
#' require(RJDemetra)
#' original_directory <- getwd()
#' extdata_directory  <- system.file("extdata", package = "RJDProcessor")
#' setwd(extdata_directory)
#' input_workspace <- "workspace_test/workspace.xml"
#' virtual_workspace <- load_workspace(input_workspace)
#' compute(virtual_workspace)
#' m <- get_jmodel(virtual_workspace)
#' r_model_list <- get_r_model_from_j_model(m)
#' setwd(original_directory)
#' @export
get_r_model_from_j_model <- function(j_model)
{
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

#' Compare the same models contained in two workspaces
#'
#' This function compares the same models contained in two workspaces (old and new)
#' plotting the respective sasonally adjusted series into a pdf file
#'
#' @param new_r_model -optional- an R model obtained via RJDemetra::get_model(workspace).
#'                    If != NA, this model is used as new model in the comparison.
#'                    Default = NA, new_model_workspace is used as new model
#' @param new_model_workspace the workspace (relative/absolute) path of the .xml
#'                    file or object used as new model in the comparison
#' @param old_model_workspace the workspace (relative/absolute) path of the .xml
#'                    file or object used as old model in the comparison
#' @param materialized_ws_new -optional- Default=FALSE, boolean field stating
#'                    whether the new workspace in the comparison is passed as
#'                    a matherialized or a virtual workspace (i.e. R object)
#' @param materialized_ws_old -optional- Default=TRUE, boolean field stating
#'                    whether the new workspace in the comparison is passed as
#'                    a matherialized or a virtual workspace (i.e. R object)
#' @param java_processing_old_model -optional- Default=TRUE, use Java models for
#'                    the internal processing of the old workspace (faster than R)
#' @return a "comparisons.pdf" file containig the plots of the seasonally adjusted
#'         time series of both the new and old workspaces. Series with the same
#'         series_name are reported in the same plot
#' @examples
#' require(RJDemetra)
#' original_directory <- getwd()
#' extdata_directory  <- system.file("extdata", package = "RJDProcessor")
#' setwd(extdata_directory)
#' input_workspace_materialized <- "workspace_test/workspace.xml"
#' virtual_workspace <- load_workspace(input_workspace_materialized)
#' compute(virtual_workspace)
#' compare_sa_ts(new_model_workspace = virtual_workspace,
#'               old_model_workspace = input_workspace_materialized ,
#'                materialized_ws_new=FALSE, materialized_ws_old=TRUE,
#'                java_processing_old_model=FALSE)
#' setwd(original_directory)
#' @export
compare_sa_ts <- function(new_r_model=NA, new_model_workspace, old_model_workspace, materialized_ws_new=FALSE, materialized_ws_old=TRUE, java_processing_old_model=TRUE)
{
  require(RJDemetra)

  #################### Prepare model_new and model_old #########################

  r_new_models_loaded <- FALSE
  #browser()
  if(!is.na(new_r_model))
  {
    r_new_models_loaded <- TRUE
    model_new <- new_r_model
  }
  else
  {
    if(materialized_ws_new == FALSE)
    {
      compute(new_model_workspace)
      j_model_new <- get_jmodel(new_model_workspace)
      model_new <- j_model_new
    } else
    {
      j_ws_new <- RJDemetra::load_workspace(new_model_workspace)
      compute(j_ws_new)
      j_model_new <- get_jmodel(j_ws_new)
      model_new <- j_model_new
    }
    #model_new <- get_r_model_from_j_model(j_model_new)
  }

  if(materialized_ws_old == FALSE)
  {
    compute(j_ws_old)
    j_model_old <- get_jmodel(old_model_workspace)
    model_old <- j_model_old
  } else
  {
    j_ws_old <- RJDemetra::load_workspace(old_model_workspace)
    compute(j_ws_old)
    j_model_old <- get_jmodel(j_ws_old)
    model_old <- j_model_old
  }

  if(java_processing_old_model==TRUE)
  {
    model_old_j <- get_jmodel(j_ws_old)
    #model_old   <- get_r_model_from_j_model(model_old_j)
    model_old <- model_old_j
  } else
  {
    model_old <- get_model(j_ws_old)
  }


  pdf(file="comparisons.pdf")

  i=1
  series_names = names(model_new[[1]])

  for(series in model_new[[1]])
  {
    # new_model_ts   <- series$final$series[,"sa"]
    # new_model_name <- series_names[i]
    #
    # browser()
    # if(java_processing_old_model==TRUE)
    # {
    #   old_model_ts <- 1
    #   old_models   <- 1
    # }  else # R processing
    # {
    #   old_model_ts <- model_old[[1]][[new_model_name]]$final$series[,"sa"]
    #   old_models   <- model_old[[1]][[new_model_name]]
    # }

    ######################## Adapt to R or Java models #########################

    #browser()


    new_model_name <- series_names[i]

    if(r_new_models_loaded) # model_new is in R
    {
      new_model_ts   <- series$final$series[,"sa"]
    }
    else # model_new is in Java
    {
      new_model_ts <- RJDemetra::get_indicators(model_new[[1]][[new_model_name]], "sa")[[1]]
    }

    if(java_processing_old_model==FALSE) # model_old is in R
    {
      old_model_ts <- model_old[[1]][[new_model_name]]$final$series[,"sa"]
      old_models   <- model_old[[1]][[new_model_name]]
      if(is.null(old_models))
      {
        old_models <- NA
      }
    }
    else # model_old is in Java
    {
      old_model_ts <- RJDemetra::get_indicators(model_old[[1]][[new_model_name]], "sa")[[1]]
      old_models   <- model_old[[1]][[new_model_name]]
      if(is.null(old_models))
      {
        old_models <- NA
      }
    }
    ################################ Plots #####################################




    plot(new_model_ts,  col = "blue", lty = 1, ylab = "Values", xlab = "Time", main = new_model_name)
    if(!is.null(old_models) && !all(is.na(old_models)))
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

