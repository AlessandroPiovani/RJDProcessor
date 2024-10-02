
#install.packages("rjdworkspace")


adjust_xml_external_variables_read_from_plugin <- function(file_path) {
  xml_content <- readLines(file_path, warn = FALSE)
  modify_string <- function(line) {
    gsub("_ts_external_(\\d+)\\.(\\w+)", "r._ts_external_\\1@\\2", line)
  }
  modified_content <- sapply(xml_content, modify_string, USE.NAMES = FALSE)
  writeLines(modified_content, file_path)
}

#' @export
get_single_ts_workspaces <- function(full_workspace, single_workspaces_path, compressed_ws=TRUE, clean_single_ws_directory=TRUE, from_TS_PLUS_plugin=TRUE)
{
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


#' @export
check_data <- function(raw_data, ws_single_ts, raw_data_start=NA, raw_data_freq=NA)
{
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

#' @export
check_external_regressors <- function(ws_single_ts)
{
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


#' @export
merge_workspaces <- function(source_workspaces_path, merged_ws_name = "merged_ws", merged_ws_dir=NA, compressed = TRUE, delete_originals = TRUE, silent=TRUE)
{
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



#' @export
update_data <- function(workspace_xml_path, data_reader)
{
  #browser()
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


# utility function
get_sa_spec <-function(class, ts_info, new_data)
{

  class <- tolower(class)

  contains_tramoseats <- grepl("tramoseats", class)
  contains_x13 <- grepl("x13", class)
  contains_regarima <- grepl("regarima", class)

  rSA    <- jSA2R(ts_info)

  if (contains_regarima && contains_tramoseats) {
          suppressWarnings({
            spec <- RJDemetra::regarima_spec_tramoseats(spec=rSA)
            spec_with_data <- regarima_tramoseats(series= new_data , spec = spec)
          })
  } else if (contains_regarima && contains_x13) {
          suppressWarnings({
                spec <- RJDemetra::regarima_spec_x13(spec=rSA)
                spec_with_data <- regarima_x13(series= new_data , spec = spec)
          })
  } else if (contains_tramoseats) {
          suppressWarnings({
               spec <- RJDemetra::tramoseats_spec(spec=rSA)
               spec_with_data <- tramoseats(series= new_data , spec = spec)
          })
  } else if (contains_x13) {
          suppressWarnings({
                spec <- RJDemetra::x13_spec(spec=rSA)
                spec_with_data <- x13(series= new_data , spec = spec)
          })
  } else if (contains_regarima) {
          suppressWarnings({
                spec <- RJDemetra::regarima_spec_x13(spec=rSA)
                spec_with_data <- regarima(series= new_data , spec = spec)
          })
  }

  return(spec_with_data)
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
