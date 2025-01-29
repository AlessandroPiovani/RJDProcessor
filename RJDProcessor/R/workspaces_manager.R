
#install.packages("rjdworkspace")


adjust_xml_external_variables_read_from_plugin <- function(file_path) {
  xml_content <- readLines(file_path, warn = FALSE)
  modify_string <- function(line) {
    gsub("_ts_external_(\\d+)\\.(\\w+)", "r._ts_external_\\1@\\2", line)
  }
  modified_content <- sapply(xml_content, modify_string, USE.NAMES = FALSE)
  writeLines(modified_content, file_path)
}


#' Get a single workspace for each series in a workspace
#'
#' This function gets a single time series workspace for each time series SA model
#' present in a given workspace
#'
#' @param full_workspace A workspace with multiple time series SA models
#' @param single_workspaces_path The path in which the single workspcaes will be
#'                      stored
#' @param compressed_ws -optional- Default=TRUE, compress the single workspaces
#'                      that will be created
#' @param clean_single_ws_directory -optional- Default=TRUE, delete the original
#'                      workspace after creating the single series ones
#' @param from_TS_PLUS_plugin -optional- Default=TRUE, the original workspace is
#'                      created by the SA_ext_plugin. In that case some internal
#'                      adjustments are needed and they could make the computation
#'                      slower. There are no problems if this field is TRUE but
#'                      the ws is created without SA_ext_plugin
#' @return A set of materialized workspaces in single_workspaces_path, one workspace
#'         for each time series SA model contained in full_workspace
#' @examples
#' require(RJDemetra)
#' original_directory <- getwd()
#' extdata_directory  <- system.file("extdata", package = "RJDProcessor")
#' setwd(extdata_directory)
#' single_workspaces_path <- "splitted_workspaces"
#' full_workspace         <- "WorkspaceTUR-container\\workspace-TUR.xml"
#' compressed_ws          <-  FALSE
#' get_single_ts_workspaces(full_workspace, single_workspaces_path,
#'                          compressed_ws = compressed_ws )
#' setwd(original_directory)
#' @export
get_single_ts_workspaces <- function(full_workspace, single_workspaces_path, compressed_ws=TRUE, clean_single_ws_directory=TRUE, from_TS_PLUS_plugin=TRUE)
{
  require(RJDemetra)

  #browser()
  remove_full_ws_in_the_end <- FALSE


  if(!require(rjdworkspace))
  {
    print("This function requires rjdworkspace package, make sure to have it installed on your PC to use it")
    return(NULL)
  }

  if (!dir.exists(single_workspaces_path))
  {
    dir.create(single_workspaces_path, recursive = TRUE)
    message("Directory created: ", single_workspaces_path)
  } else
  {
    if (clean_single_ws_directory) {

      files_to_delete <- list.files(single_workspaces_path, full.names = TRUE)
      normalized_path_single_ws <- normalizePath(single_workspaces_path, winslash = "\\", mustWork = FALSE)
      normalized_path_full_ws   <- normalizePath(dirname(full_workspace), winslash = "\\", mustWork = FALSE)
      normalized_path_full_ws_container  <- normalizePath(dirname(dirname(full_workspace)), winslash = "\\", mustWork = FALSE)


      if(normalized_path_single_ws == normalized_path_full_ws_container)
      {
        remove_full_ws_in_the_end <- TRUE

        normalized_files_to_delete <- normalizePath(files_to_delete, winslash = "\\", mustWork = FALSE)
        index_to_remove <- which(normalized_files_to_delete == normalized_path_full_ws)
        if (length(index_to_remove) > 0) {
          files_to_delete <- files_to_delete[-index_to_remove]
        }
      }

      unlink(files_to_delete, recursive = TRUE, force = TRUE)
    }
  }

  ws_full<-load_workspace(file = full_workspace)
  compute(ws_full)
  j_multiprocessing_list <- get_jmodel(ws_full)

  #for(j_multiproc in j_multiprocessing_list)
  for(multiproc_name in names(j_multiprocessing_list))
  {
    j_multiproc<-j_multiprocessing_list[[multiproc_name]]
    for(ts_name in names(j_multiproc))
    {
      time_series_info <- j_multiproc[[ts_name]]

      ws_single_ts <- new_workspace()
      mp_single_ts <- new_multiprocessing(ws_single_ts, multiproc_name)
      add_sa_item(workspace = ws_single_ts, multiprocessing = multiproc_name, sa_obj = time_series_info, name = ts_name)
      compute(ws_single_ts)
      #add_new_sa_item(sap, sa_item)

      ts_path      <- file.path(single_workspaces_path, ts_name)
      ts_file_path <- file.path(ts_path, paste0(ts_name, ".xml"))

      #browser()

      if (!dir.exists(ts_path))
      {
        dir.create(ts_path, recursive = TRUE)
      }
      #compute(ws_single_ts) #Added --> when saved the workspace is unprocessed anyway
      save_workspace(ws_single_ts, ts_file_path)

      if(from_TS_PLUS_plugin)
      {
        #multiproc_root <- sub("-\\d+$", "", multiproc_name)
        multiproc_root <- "SAProcessing"
        file_path <- file.path(single_workspaces_path, ts_name, ts_name, multiproc_root, paste0(multiproc_name,".xml"))
        adjust_xml_external_variables_read_from_plugin(file_path)
        #in SAProcessing (namely in multiproc_name) modify variables such as:
        #_ts_external_1@PENT_DUR6_IEM_0 in r._ts_external_1@PENT_DUR6_IEM_0
      }

    }
  }
  if(remove_full_ws_in_the_end == TRUE)
  {
    unlink(dirname(full_workspace), recursive = TRUE, force = TRUE)
  }

  if (compressed_ws)
  {
    dirs_to_compress <- list.dirs(single_workspaces_path, full.names = TRUE, recursive = FALSE)

    #browser()
    for (dir in dirs_to_compress)
    {
      dir_name<-basename(dir)
      zip_file <- paste0(dir_name, ".zip")

      original_dir <- getwd()
      setwd(single_workspaces_path)

      system(sprintf("zip -r -q %s %s", zip_file, dir_name), intern = TRUE)

      setwd(original_dir)


      #zip(zip_file, dir)


      unlink(dir, recursive = TRUE, force = TRUE)
    }
  }

}


#' Compare the data in a workspace with only one time series with given data
#'
#' This function compares the data in a workspace with only one time series with
#' given raw data in array form
#'
#' @param raw_data     Raw data to be compared with the ones in the workspace
#' @param ws_single_ts The xml path of the workspace containing the data to be
#'                     compared
#' @param raw_data_start -optional- Default=NA, the starting date of the raw data
#'                     in form "YYYY-MM-DD". If NA, the starting date is assumed
#'                     to be the same as the workspace data
#' @param raw_data_freq -optional- Default=NA, the frequency of the raw data (
#'                      e.g. 12=monthly, 4=quarterly). If NA, the frequency is
#'                      assumed to be the same as the workspace data
#' @return Boolean: TRUE if data are the same, FALSE otherwise
#' @examples
#' require(RJDemetra)
#' original_directory <- getwd()
#' extdata_directory  <- system.file("extdata", package = "RJDProcessor")
#' setwd(extdata_directory)
#' single_workspaces_path <- "splitted_workspaces_to_merge"
#' single_ws<-load_workspace(paste0(single_workspaces_path,"\\VATPIC\\VATPIC.xml"))
#' result<-check_data(c(1,2,3), single_ws)
#' setwd(original_directory)
#' @export
check_data <- function(raw_data, ws_single_ts, raw_data_start=NA, raw_data_freq=NA)
{
  require(RJDemetra)

  #browser()

  compute(ws_single_ts)
  # mp_single_ts_list  <- get_jmodel(ws_single_ts)
  # mp_single_ts       <- mp_single_ts_list[[1]]
  # sa_item       <- .jcall(mp_single_ts, "Lec/tstoolkit/jdr/ws/SaItem;", "get", as.integer(0))
  # sa_definition <- .jcall(sa_item, "Ljd2/datatypes/sa/SaItemType;", "getSaDefinition")
  # ts<-sa_definition$getTs()
  # data<-ts$getData()
  # v<-data$get(as.integer(0))
  # data_vals<-data$internalStorage()


  multiproc_list<-get_ts(ws_single_ts)
  timeser<-multiproc_list[[1]][[1]]


  same_length  <- (length(raw_data) == length(timeser))
  if(!same_length){ message("different lengths between target data and workspace data!")}

  equal_values <- isTRUE(all.equal(raw_data, timeser))
  if(!equal_values){ message("different values between target data and workspace data!")}


  same_start <- TRUE
  if(!is.na(raw_data_start))
  {
    start <- start(timeser) # e.g. c(2010,1)
    same_start <- (start[1]==raw_data_start[1] && start[2]==raw_data_start[2]) # TODO
    if(!same_start){ message("different start!")}

  }

  same_freq  <- TRUE
  if(!is.na(raw_data_freq))
  {
    same_freq <- (frequency(timeser) ==  raw_data_freq)# TODO
    if(!same_freq){ message("different frequencies!")}

  }

  return(same_length && equal_values && same_freq && same_start)

}



#' Verify the external regressors used in a workspace
#'
#' This function check whether the external regressors used in a workspace are
#' up to date (i.e. they cover from the beginning to the end of the time series
#' and have the same frequency). The workspace must contain a single time series.
#'
#' @param ws_single_ts The xml path of the workspace containing the data to be
#'                     checked
#' @return Boolean: TRUE if ckeck is ok, FALSE if there are problems in the
#'         external regressors.
#' @examples
#' require(RJDemetra)
#' original_directory <- getwd()
#' extdata_directory  <- system.file("extdata", package = "RJDProcessor")
#' setwd(extdata_directory)
#' single_workspaces_path <- "splitted_workspaces_to_merge"
#' single_ws<-load_workspace(paste0(single_workspaces_path,"\\VATPIC\\VATPIC.xml"))
#' compute(single_ws)
#' result<- check_external_regressors(single_ws)
#' setwd(original_directory)
#' @export
check_external_regressors <- function(ws_single_ts)
{
  require(RJDemetra)

  #browser()
  multiproc_list <- get_ts(ws_single_ts)
  ts_data        <- multiproc_list[[1]][[1]]
  m <- get_model(ws_single_ts)
  ret            <- TRUE

  if(m[[1]][[1]][["regarima"]][["specification"]][["regression"]][["userdef"]][["specification"]][["variables"]]==TRUE)
  {
    series<-m[[1]][[1]][["regarima"]][["specification"]][["regression"]][["userdef"]][["variables"]][["series"]]

    for(i in 1:ncol(series)) # compare data to be seasonally adjusted with external regressors to ensure they are suitable (include all the data dates)
    {
      ext_reg_ser <- series[,i]

      # check for the beginning of the external regressor
      if(start(ext_reg_ser)[1]<start(ts_data)[1]) #compare year of start
      {
        ret<-TRUE
      }else if(start(ext_reg_ser)[1]==start(ts_data)[1])# check month
      {
        if(start(ext_reg_ser)[1]<=start(ts_data)[1]) { ret<- TRUE }
        else{
          message("External regressor too short detected")
          return(FALSE)
        }
      }
      else
      {
        message("External regressor too short detected")
        return(FALSE)
      }

      # check for the end of the external regressor
      if(end(ext_reg_ser)[1]>start(ts_data)[1]) #compare year of end
      {
        ret<-TRUE
      }else if(start(ext_reg_ser)[1]==start(ts_data)[1])# check month
      {
        if(start(ext_reg_ser)[1]>=start(ts_data)[1]) { ret<- TRUE }
        else{
          message("External regressor too short detected")
          return(FALSE)
        }
      }
      else
      {
        message("External regressor too short detected")
        return(FALSE)
      }


      # check for the frequency of the external regressor
      if(frequency(ext_reg_ser)!= frequency(ext_reg_ser))
      {
        message("External regressor with different frequency than data detected")
        return(FALSE)
      }
    }


  }
  return(ret)

}


#' Merge many workspaces in one
#'
#' This function gets merges many workspaces contained in a given folder into one
#' workspace
#'
#' @param source_workspaces_path The path in which to find all the workspcaes that
#'                       will be merged
#' @param merged_ws_name -optional- Default="merged_ws". The name of the workspace
#'                       that will be created
#' @param merged_ws_dir  -optional- Default=NA, the path in which the merged workspace
#'                       will be stored. If NA it will be stored in the current
#'                       directory
#' @param compressed     -optional- Default=TRUE, workspaces to be merged are
#'                       compressed
#' @param delete_originals -optional- Default=TRUE, delete original workspaces
#'                         after merging them
#' @param silent -optional- Default=TRUE, do not print status bar and messages
#'               during the operations
#' @return A virtual (R) and a marerialized workspace containing all the
#'         multiprocessings and time series SA models of the original workspaces
#' @examples
#' require(RJDemetra)
#' original_directory <- getwd()
#' extdata_directory  <- system.file("extdata", package = "RJDProcessor")
#' setwd(extdata_directory)
#' single_workspaces_path <- "splitted_workspaces_to_merge"
#' full_workspace_path    <- "merged_ws_test"
#' compressed_ws          <-  FALSE
#' ws_merged <- merge_workspaces(source_workspaces_path=single_workspaces_path,
#'              merged_ws_dir = full_workspace_path, compressed = TRUE,
#'              delete_originals = FALSE, silent=TRUE)
#'
#' setwd(original_directory)
#' @export
merge_workspaces <- function(source_workspaces_path, merged_ws_name = "merged_ws", merged_ws_dir=NA, compressed = TRUE, delete_originals = TRUE, silent=TRUE)
{
  require(RJDemetra)

  # make sure that workspaces' path does not end with "/"
  source_workspaces_path <- sub("/$", "", source_workspaces_path)

  if(is.na(merged_ws_dir))
  {
    merged_ws_path <- file.path(source_workspaces_path, merged_ws_name)
    # if (dir.exists(merged_ws_path)) { #modified
    #   unlink(merged_ws_path, recursive = TRUE)
    # }
  }else # modified
  {
    merged_ws_path <- file.path(merged_ws_dir, merged_ws_name)
  }

  #browser()
  # if compressed == TRUE, unzip all the compressed folders (.zip)
  if (compressed) {
    zip_files <- list.files(source_workspaces_path, pattern = "\\.zip$", full.names = TRUE)

    for (zip_file in zip_files) {
      unzip(zip_file, exdir = source_workspaces_path)
      file.remove(zip_file)
    }
  }

  dirs <- list.dirs(source_workspaces_path, full.names = TRUE, recursive = FALSE)

  if (!dir.exists(merged_ws_path)) #modified
  {
    dir.create(merged_ws_path, recursive = TRUE)
  }
  ws_merged <- new_workspace()
  mp_merged <- new_multiprocessing(ws_merged, "sa1")

  #browser()

  for(ws_dir in dirs)
  {
    # Find XML file in ws_dir
    xml_files <- list.files(ws_dir, pattern = "\\.xml$", full.names = TRUE)

    # Only one XML is supposed to be present
    ws_name <- xml_files[1]

    single_ws <- load_workspace(ws_name)

    #browser()

    compute(single_ws)
    j_multiprocessing_list <- get_jmodel(single_ws, progress_bar = !silent)

    #for(j_multiproc in j_multiprocessing_list)
    for(multiproc_name in names(j_multiprocessing_list))
    {
      j_multiproc<-j_multiprocessing_list[[multiproc_name]]
      for(ts_name in names(j_multiproc))
      {
        time_series_info <- j_multiproc[[ts_name]]

        #add_new_sa_item(sap = mp_merged, sa_item = time_series_info)
        add_sa_item(workspace = ws_merged, multiprocessing = "sa1", sa_obj = time_series_info, name = ts_name)
      }
    }

    # the following lines should have worked instead of the previous ones, but they does not copy external variables
    # mps <- get_all_objects(single_ws)
    # for(multiproc in mps)
    # {
    #   for(sa_items in multiproc)
    #   {
    #     for(sa_item in sa_items)
    #     {
    #       add_new_sa_item(sap = mp_merged, sa_item = sa_item)
    #     }
    #   }
    # }

    # suppress the warnings?
    #save_workspace(ws_merged, paste0(merged_ws_path,"/",merged_ws_name,".xml"))
  }
  # suppress the warnings?
  #compute(ws_merged) #Added --> when saved the workspace is unprocessed anyway
  save_workspace(ws_merged, paste0(merged_ws_path,"/",merged_ws_name,".xml"))


  # if delete_originals == TRUE, deletes original workspaces
  if (delete_originals) {
    original_dirs <- list.dirs(source_workspaces_path, full.names = TRUE, recursive = FALSE)

    # Filter the merged ws directory just created
    original_dirs <- original_dirs[basename(original_dirs) != merged_ws_name]

    # Delete original directories
    for (dir in original_dirs) {
      unlink(dir, recursive = TRUE)
    }
  }
  return(ws_merged)

}


#' Update the data of a workspace
#'
#' This function update the data of a workspace's time series basing on the data
#' read by a Data_reader object already initialized. The time series read by the
#' Data_reader must have the same colnames as the time series names of the workspace
#' to produce an update
#'
#' @param workspace_xml_path Path of the xml file of the workspace whose data
#'                           have to be updated
#' @param data_reader A Data_reader object already initialized with its input
#'                    source
#' @examples
#' require(RJDemetra)
#' original_directory <- getwd()
#' extdata_directory  <- system.file("extdata", package = "RJDProcessor")
#' setwd(extdata_directory)
#' ws_xml_path  <- "TUR_ws_test_container/merged_ws.xml"
#' dr <- RJDProcessor::Data_reader_csv(input_source = "rawdata_TUR.csv")
#' # num_mat<-dr@read_data() # to check if the data are available
#' update_data(ws_xml_path, dr)
#' setwd(original_directory)
#' @export
#'
#'
#'
update_data <- function(workspace_xml_path, data_reader)
{
  require(RJDemetra)

  ws_path          <- dirname(workspace_xml_path)
  temp_new_ws_path <- paste0(ws_path, "_temp")


  time_series <- data_reader@read_data()
  time_series <- get_mts(time_series)


  workspace <- load_workspace(workspace_xml_path)
  new_ws    <- RJDemetra::new_workspace()


  compute(workspace)

  jmplist<-get_jmodel(workspace)

  #for(mp in jmplist)
  pos_mp=1
  for(mp_name in names(jmplist))
  {
    mp     <- jmplist[[mp_name]]
    mp_new <- new_multiprocessing(new_ws, mp_name)
    pos_sa=1
    for(ts_name in names(mp))
    {
      ts_info <- mp[[ts_name]]
      class   <- class(ts_info$result)
      if(ts_name %in% colnames(time_series))
      {
        new_data <- time_series[, ts_name]

        spec_and_data <- get_sa_spec(class, ts_info, new_data)

        add_sa_item(workspace = new_ws, multiprocessing = mp_name, name = ts_name, sa_obj = spec_and_data)
      }
      else #series with data not present in file
      {
        multiproc_list  <- get_ts(ws_single_ts)
        timeser         <- multiproc_list[[mp_name]][[ts_name]]
        spec_and_data   <- get_sa_spec(class, ts_info, timeser)
        add_sa_item(workspace = new_ws, multiprocessing = mp_name, name = ts_name, sa_obj = spec_and_data)
      }
      pos_sa=pos_sa+1
    }
    pos_mp=pos_mp+1
  }
  #browser()
  if (!dir.exists(temp_new_ws_path)) {
    dir.create(temp_new_ws_path)
  }

  compute(new_ws)
  save_workspace(workspace = new_ws, file = paste0(temp_new_ws_path, "//", basename(workspace_xml_path)))

  # detìletes the original ws ...
  unlink(ws_path, recursive = TRUE)
  # ... and rename the new ws like him
  file.rename(temp_new_ws_path, ws_path)

}




# utility functions
ts_r2jd<-function(s){
  require(rJava)
  freq<-frequency(s)
  start<-start(s)
  jd_freq<-.jcall("ec/tstoolkit/timeseries/simplets/TsFrequency", "Lec/tstoolkit/timeseries/simplets/TsFrequency;", "valueOf", as.integer(freq))
  jd_period<-.jnew("ec/tstoolkit/timeseries/simplets/TsPeriod", jd_freq, as.integer(start[1]), as.integer(start[2]-1))
  ts<-.jnew("ec/tstoolkit/timeseries/simplets/TsData", jd_period, as.double(s), FALSE)
  return (ts)
}


get_sa_spec <-function(class, ts_info, new_data)
{
  require(rJava)
  class <- tolower(class)
  # browser()
  contains_tramoseats <- grepl("tramoseats", class)
  contains_x13 <- grepl("x13", class)
  contains_regarima <- grepl("regarima", class)

  # rSA    <- jSA2R(ts_info)

  # if (contains_regarima && contains_tramoseats) {
  #   suppressWarnings({
  #     spec <- RJDemetra::regarima_spec_tramoseats(spec=rSA)
  #     spec_with_data <- regarima_tramoseats(series= new_data , spec = spec)
  #   })
  # } else if (contains_regarima && contains_x13) {
  #   suppressWarnings({
  #     spec <- RJDemetra::regarima_spec_x13(spec=rSA)
  #     spec_with_data <- regarima_x13(series= new_data , spec = spec)
  #   })
  # } else if (contains_tramoseats) {
  if (contains_tramoseats) {
    suppressWarnings({

      # browser()

      # jrspec <- .jcall("jdr/spec/tramoseats/TramoSeatsSpec", "Ljdr/spec/tramoseats/TramoSeatsSpec;", "of", "RSA0")
      # jspec  <- .jcall(jrspec, "Lec/satoolkit/tramoseats/TramoSeatsSpecification;", "getCore")
      jspec  <- .jcall(ts_info$spec, "Lec/satoolkit/tramoseats/TramoSeatsSpecification;", "getCore")
      jrslt  <- .jcall("ec/tstoolkit/jdr/sa/Processor", "Lec/tstoolkit/jdr/sa/TramoSeatsResults;", "tramoseats", ts_r2jd(new_data), jspec,  ts_info$dictionary)
      jrslt  <- new("TramoSeats_java", internal = jrslt)

      ts_info$result <- jrslt
      spec_with_data <- ts_info


      # old way: does not work for Java modified objects (e.g. with Ramos or Intervention Variables)
      # spec <- RJDemetra::tramoseats_spec(spec=rSA)
      # spec_with_data <- tramoseats(series= new_data , spec = spec)
    })
  }
  # else if (contains_x13) {
  #   suppressWarnings({
  #     spec <- RJDemetra::x13_spec(spec=rSA)
  #     spec_with_data <- x13(series= new_data , spec = spec)
  #   })
  # } else if (contains_regarima) {
  #   suppressWarnings({
  #     spec <- RJDemetra::regarima_spec_x13(spec=rSA)
  #     spec_with_data <- regarima(series= new_data , spec = spec)
  #   })
  # }

  return(spec_with_data)
}





#' Compare two RJDemetra workspaces
#'
#' This function compares two RJDemetra workspaces, either provided as objects
#' or file paths, and generates a report of the differences in the final
#' seasonally adjusted series and likelihood BIC values.
#'
#' @param workspace1 Either an RJDemetra workspace object or a file path to a workspace.
#' @param patworkspace2 Either an RJDemetra workspace object or a file path to a workspace.
#' @param output_file Character string specifying the path of the output report file.
#' Default is "differences_report.txt".
#'
#' @return The function does not return a value but writes a report to the specified file.
#'
#' @details The function loads the workspaces if they are provided as file paths,
#' computes the models, and extracts the seasonally adjusted series and likelihood BIC.
#' It then compares these values and records any differences in the output file.
#'
#' Differences are checked for the last 36 observations of the seasonally adjusted series
#' and the likelihood BIC values.
#'
#' @export
compare_workspaces <- function(workspace1, workspace2, output_file="differences_report.txt")
{
  require(RJDemetra)
  require(RJDProcessor)

  # Caricare i workspace se sono dei file
  if (is.character(workspace1) && file.exists(workspace1)) {
    workspace1 <- load_workspace(workspace1)
  }

  if (is.character(workspace2) && file.exists(workspace2)) {
    workspace2 <- load_workspace(workspace2)
  }

  RJDemetra::compute(workspace1)
  RJDemetra::compute(workspace2)

  multiprocessing <- RJDemetra::get_jmodel(workspace1)
  ts_names<- names(multiprocessing[[1]])
  reference_results <- multiprocessing[[1]]
  rm(multiprocessing)

  multiprocessing2 <- RJDemetra::get_jmodel(workspace2)
  ts_names2<- names(multiprocessing2[[1]])
  db_results <- multiprocessing2[[1]]
  rm(multiprocessing2)




  # Inizializzo il file di report
  filename <- output_file
  con <- file(filename, open = "w")

  # Scrivo l'intestazione del report
  cat("Rapporto sulle differenze tra le serie temporali destagionalizzate\n", file = con)

  # Variabile per controllare se ci sono differenze
  ci_sono_differenze <- FALSE

  # Ciclo per ogni serie temporale
  for(ts_name in ts_names) {
    #browser()

    ########################## Serie lette da workspace1 ##########################
    #sa_values_ts <- reference_results[[ts_name]]$final$series[,"sa"]
    sa_values_ts <- RJDemetra::get_indicators(reference_results[[ts_name]], "sa")[[1]]

    sa_values_reference <- as.vector(sa_values_ts)
    sa_values_reference <- tail(sa_values_reference, 36)  # prendo solo gli ultimi 36 elementi

    sa_dates_reference <- time(sa_values_ts)
    years <- floor(sa_dates_reference)
    months <- as.numeric(round((sa_dates_reference - years) * 12) + 1)
    sa_dates_reference <- format(as.Date(paste(years, months, "01", sep = "-")), "%d-%m-%Y")
    sa_dates_reference <- tail(sa_dates_reference, 36)  # prendo solo gli ultimi 36 elementi

    #likelihoodbic_reference <- reference_results[[ts_name]]$regarima$loglik["bicc",]
    likelihoodbic_reference <- RJDemetra::get_indicators(reference_results[[ts_name]], "preprocessing.likelihood.bicc")[[1]]


    ##################### Serie lette da db (workspace2) #########################


    #sa_values_ts <- db_results[[ts_name]]$final$series[,"sa"]
    sa_values_db <- RJDemetra::get_indicators(db_results[[ts_name]], "sa")[[1]]

    sa_values <- as.vector(sa_values_db)
    sa_values <- tail(sa_values, 36)  # prendo solo gli ultimi 36 elementi

    sa_dates <- time(sa_values_db)
    years2   <- floor(sa_dates)
    months2  <- as.numeric(round((sa_dates - years) * 12) + 1)
    sa_dates <- format(as.Date(paste(years2, months2, "01", sep = "-")), "%d-%m-%Y")
    sa_dates <- tail(sa_dates, 36)  # prendo solo gli ultimi 36 elementi

    #likelihoodbic_reference <- reference_results[[ts_name]]$regarima$loglik["bicc",]
    likelihoodbic <- RJDemetra::get_indicators(db_results[[ts_name]], "preprocessing.likelihood.bicc")[[1]]


    ###################### Confronti ######################

    # Variabili per le differenze
    differenze <- ""

    # Confronto dei valori 'sa'
    if (!isTRUE(all.equal(sa_values_reference, sa_values, tolerance = 1e-9))) {
      differenze <- paste(differenze, "valori", sep = "/")
    }

    # Confronto delle date 'sa_dates'
    if (!identical(sa_dates_reference, sa_dates)) {
      differenze <- paste(differenze, "date", sep = "/")
    }

    # Confronto di 'likelihoodbic'
    #if (!identical(likelihoodbic_reference, likelihoodbic)) {
    if (!isTRUE(all.equal(sa_values_reference, sa_values, tolerance = 1e-9)))
    {
      differenze <- paste(differenze, "likelihoodbic", sep = "/")
    }

    # Se ci sono differenze, aggiorno la variabile e stampo a video
    if (differenze != "") {
      if (!ci_sono_differenze) {
        print("DIFFERENCES ARE PRESENT")
        cat("DIFFERENCES ARE PRESENT\n", file = con)  # Scrivo solo una volta nel report
        ci_sono_differenze <- TRUE
      }

      # Stampa a video del messaggio per ogni serie
      print(paste(ts_name, "differs for", substr(differenze, 2, nchar(differenze))))

      # Scrivo nel report le differenze dettagliate
      cat(paste("Differences for the series: ", ts_name, "\n", sep = ""), file = con)
      cat("------------------------------------------------------------\n", file = con)

      # Calcolo la larghezza massima per ogni colonna (per allineare correttamente)
      max_date_len <- max(nchar(sa_dates_reference), nchar(sa_dates), nchar("dates_series_ws1"), nchar("dates_series_ws2"))
      max_value_len <- max(nchar(format(sa_values_reference, scientific = FALSE)),
                           nchar(format(sa_values, scientific = FALSE)),
                           nchar("values_series_ws1"), nchar("values_series_ws2"))
      max_x_len <- max(nchar("X"), nchar(" "), nchar("non_corresponding_dates"), nchar("non_corresponding_values"))

      # Scrivo l'intestazione della tabella con una formattazione ben allineata
      cat(sprintf("%-*s| %-*s| %-*s| %-*s| %-*s| %-*s\n",
                  max_date_len, "dates_series_ws1", max_value_len, "values_series_ws1",
                  max_date_len, "dates_series_ws2", max_value_len, "values_series_ws2",
                  max_x_len, "non_corresponding_dates", max_x_len, "non_corresponding_values"), file = con)
      cat(rep("-", sum(c(max_date_len, max_value_len, max_date_len, max_value_len, max_x_len, max_x_len)) + 5), "\n", file = con)

      # Creazione del confronto tabellare
      for (i in 1:length(sa_values_reference)) {

        # Se ci sono differenze tra date e valori, lo segnalo con "X"
        date_diff <- ifelse(sa_dates_reference[i] != sa_dates[i], "X", " ")
        value_diff <- ifelse(sa_values_reference[i] != sa_values[i], "X", " ")

        # Scrivo nel file con una formattazione corretta per colonne allineate
        cat(sprintf("%-*s| %-*s| %-*s| %-*s| %-*s| %-*s\n",
                    max_date_len, sa_dates_reference[i], max_value_len, format(sa_values_reference[i], scientific = FALSE),
                    max_date_len, sa_dates[i], max_value_len, format(sa_values[i], scientific = FALSE),
                    max_x_len, date_diff, max_x_len, value_diff), file = con)
      }

      # Aggiungo la sezione per 'likelihoodbic'
      cat("\n", file = con)
      if (!identical(likelihoodbic_reference, likelihoodbic)) {
        cat(sprintf("different likelihoodbic: %.6f (ws1) vs %.6f (ws2)\n", likelihoodbic_reference, likelihoodbic), file = con)
      }

      cat("\n", file = con)  # Linea vuota tra i report delle serie
    }
  }

  # Se non ci sono differenze, scrivo un messaggio
  if (!ci_sono_differenze) {
    print("No differences found between the series.")
    cat("No differences found between the series.\n", file = con)
  }

  close(con)

  print(paste("The report has been written to the file ", filename, sep = ""))



}













# # TEST
# setwd("C:\\Users\\UTENTE\\Desktop\\RJDopenCruncher\\test")
#
# single_workspaces_path <- "splitted_workspaces"
# full_workspace         <- "WorkspaceTUR-container\\workspace-TUR.xml"
# #full_workspace         <- "output_workspace_container_xlsx\\workspace.xml"
# compressed_ws          <-  FALSE
#
# get_single_ts_workspaces(full_workspace, single_workspaces_path, compressed_ws = compressed_ws )
#
# single_ws<-load_workspace(paste0(single_workspaces_path,"\\VATPIC\\VATPIC.xml"))
# check_data(c(1,2,3), single_ws)
# check_external_regressors(single_ws)
#
# #browser()
#
# ws_merged           <- merge_workspaces(source_workspaces_path=single_workspaces_path, merged_ws_name = "merged_ws", compressed = TRUE, delete_originals = TRUE, silent=TRUE)
# ws_merged_xml_path  <- paste0(single_workspaces_path,"//","merged_ws","//","merged_ws",".xml")
#
#
# dr  <- RJDProcessor::Data_reader_csv_istat_format(input_source = "rawdata_ISTATformat_TUR.csv")
# dr2 <- RJDProcessor::Data_reader_csv(input_source = "rawdata_TUR.csv")
#
# #num_mat<-dr@read_data()
#
# update_data(ws_merged_xml_path, dr2)
#
