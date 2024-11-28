require(RJDemetra)


#' Create Diagnostic Report for Time Series Models
#'
#' This function generates a diagnostic report for time series models,
#' including BIC, p-values from Ljung-Box tests, and normality test results.
#' It organizes the output into categories: all series, series with Ljung-Box problems,
#' series with normality issues, and a summary of problematic series.
#'
#' @param workspace A workspace object containing the time series models.
#' @param output_file A string specifying the output file path where the report will be saved (default is "report.txt").
#' @return NULL
#' @examples
#' require(RJDemetra)
#' 
#' original_directory <- getwd()
#' extdata_directory  <- system.file("extdata", package = "RJDProcessor")
#' setwd(extdata_directory)
#' ws_path <- "WorkspaceTUR-container/workspace-TUR.xml"
#' 
#' workspace <- load_workspace(file=ws_path)
#' create_diagnostic_report1(workspace, output_file = "report.out")
#' setwd(original_directory)
#' @export
create_diagnostic_report1 <- function(workspace, output_file="report.txt") {
  # if(!requireNamespace("RJDemetra", quietly = TRUE)) {
  #      stop("You need to install RJDemetra to use this function")
  # }else{require("RJDemetra")}

  
  if (file.exists(output_file)) {
    file.remove(output_file)
  }
  
  compute(workspace)
  jmodel <- get_jmodel(workspace)
  
  report_data <- list()
  problematic_series <- list()
  idx <- 1
  for (multiprocessing in jmodel) {
    for (series_name in names(multiprocessing)) {
      time_series <- multiprocessing[[series_name]]
      
      ts <- get_indicators(x = time_series, "y")
      freq <- 12 / frequency(ts)
      
      qs           <- get_indicators(x = time_series, "diagnostics.qs")[[1]]
      model_descr  <- get_indicators(x = time_series, "preprocessing.model.description")
      model_coeffs <- get_indicators(x = time_series, "preprocessing.model.coefficients")
      model_coeffs_T_stat <- model_coeffs[[1]][, 1] / model_coeffs[[1]][, 2]
      bic <- get_indicators(x = time_series, "preprocessing.likelihood.bic")[[1]]
      normality    <- get_indicators(x = time_series, "preprocessing.residuals.dh")[[1]]
      n_outliers   <- get_indicators(x = time_series, "preprocessing.model.nout")[[1]]
      outliers <- list()
      for (i_out in 1:n_outliers) {
        o <- get_indicators(x = time_series, paste0("preprocessing.model.out(", i_out, ")"))
        outliers <- append(outliers, list(o[[1]]))
      }
      
      arimaparams <- get_indicators(x = time_series, "preprocessing.arima.parameters")[[1]]
      lb          <- get_indicators(x = time_series, "preprocessing.residuals.lb")[[1]]
      lb_squared  <- get_indicators(x = time_series, "preprocessing.residuals.lb2")[[1]]
      
      non_significant_coeffs <- model_descr[[1]][abs(model_coeffs_T_stat) < 2]
      has_non_significant <- length(non_significant_coeffs) > 0
      
      report_data[[idx]] <- data.frame(
        frequency = round(freq),
        TITLE = series_name,
        BIC = bic,
        LB_Prob = round(lb[2], 3),
        LB2_Prob = round(lb_squared[2], 3),
        Norm_Prob = round(normality[2], 3),
        Norm_Test = normality[1],
        Not_significant_coeff = if (has_non_significant) paste(non_significant_coeffs, collapse = ", ") else ""
      )
      
      if (any(c(lb[2], lb_squared[2], normality[2]) < 0.05) || has_non_significant) {
        problematic_series[[idx]] <- report_data[[idx]]
      }
      
      idx <- idx + 1
    }
  }
  
  report_df <- do.call(rbind, report_data)
  problematic_series_df <- do.call(rbind, problematic_series)
  
  lb_problems_df         <- report_df[report_df$LB_Prob < 0.05, ]
  lb_squared_problems_df <- report_df[report_df$LB2_Prob < 0.05, ]
  normality_problems_df  <- report_df[report_df$Norm_Prob < 0.05, ]
  
  header <- sprintf("%-5s %-5s %-15s %-8s %-10s %-10s %-10s %-10s\n", "", "Freq", "TITLE", "BIC", "LB_Prob", "LB2_Prob", "Norm_Prob", "Norm_Test")
  separator            <- "------------------------------------------------------------------------------------------\n"
  lb_separator         <- "-------------------------------------- LB PROBLEMS --------------------------------------\n"
  lb_squared_separator <- "---------------------------------- LB SQUARED PROBLEMS ----------------------------------\n"
  normality_separator  <- "----------------------------------- NORMALITY PROBLEMS ----------------------------------\n"
  resume_separator     <- "------------------------------------ PROBLEMS RESUME ------------------------------------\n"
  
  cat(header, file = output_file)
  cat(separator, file = output_file, append = TRUE)
  for (i in 1:nrow(report_df)) {
    cat(sprintf("%-5d %-5d %-15s %-8.2f %-10.3f %-10.3f %-10.3f %-10.2f\n",
                i,
                report_df$frequency[i],
                report_df$TITLE[i],
                report_df$BIC[i],
                report_df$LB_Prob[i],
                report_df$LB2_Prob[i],
                report_df$Norm_Prob[i],
                report_df$Norm_Test[i]),
        file = output_file, append = TRUE)
  }
  
  cat("\n\n", file = output_file, append = TRUE)
  cat(lb_separator, file = output_file, append = TRUE)
  cat(header, file = output_file, append = TRUE)
  cat(separator, file = output_file, append = TRUE)
  for (i in 1:nrow(lb_problems_df)) {
    idx <- which(rownames(report_df) == rownames(lb_problems_df)[i])
    cat(sprintf("%-5d %-5d %-15s %-8.2f %-10.3f %-10.3f %-10.3f %-10.2f\n",
                idx,
                lb_problems_df$frequency[i],
                lb_problems_df$TITLE[i],
                lb_problems_df$BIC[i],
                lb_problems_df$LB_Prob[i],
                lb_problems_df$LB2_Prob[i],
                lb_problems_df$Norm_Prob[i],
                lb_problems_df$Norm_Test[i]),
        file = output_file, append = TRUE)
  }
  
  cat("\n\n", file = output_file, append = TRUE)
  cat(normality_separator, file = output_file, append = TRUE)
  cat(header, file = output_file, append = TRUE)
  cat(separator, file = output_file, append = TRUE)
  for (i in 1:nrow(normality_problems_df)) {
    idx <- which(rownames(report_df) == rownames(normality_problems_df)[i])
    cat(sprintf("%-5d %-5d %-15s %-8.2f %-10.3f %-10.3f %-10.3f %-10.2f\n",
                idx,
                normality_problems_df$frequency[i],
                normality_problems_df$TITLE[i],
                normality_problems_df$BIC[i],
                normality_problems_df$LB_Prob[i],
                normality_problems_df$LB2_Prob[i],
                normality_problems_df$Norm_Prob[i],
                normality_problems_df$Norm_Test[i]),
        file = output_file, append = TRUE)
  }
  
  cat("\n\n", file = output_file, append = TRUE)
  cat(lb_squared_separator, file = output_file, append = TRUE)
  cat(header, file = output_file, append = TRUE)
  cat(separator, file = output_file, append = TRUE)
  for (i in 1:nrow(lb_squared_problems_df)) {
    idx <- which(rownames(report_df) == rownames(lb_squared_problems_df)[i])
    cat(sprintf("%-5d %-5d %-15s %-8.2f %-10.3f %-10.3f %-10.3f %-10.2f\n",
                idx,
                lb_squared_problems_df$frequency[i],
                lb_squared_problems_df$TITLE[i],
                lb_squared_problems_df$BIC[i],
                lb_squared_problems_df$LB_Prob[i],
                lb_squared_problems_df$LB2_Prob[i],
                lb_squared_problems_df$Norm_Prob[i],
                lb_squared_problems_df$Norm_Test[i]),
        file = output_file, append = TRUE)
  }
  
  cat("\n\n", file = output_file, append = TRUE)
  
  # PROBLEMS RESUME: only problematic series
  cat(resume_separator, file = output_file, append = TRUE)
  cat(sprintf("%-5s %-5s %-15s %-8s %-10s %-10s %-10s %-10s %-25s\n", "", "Freq", "TITLE", "BIC", "LB_Prob", "LB2_Prob", "Norm_Prob", "Norm_Test", "Not_significant_coeff"), file = output_file, append = TRUE)
  cat("--------------------------------------------------------------------------------------------------------------\n", file = output_file, append = TRUE)
  
  for (i in 1:nrow(problematic_series_df)) {
    lb_prob_display <- ifelse(problematic_series_df$LB_Prob[i] < 0.05, paste0(problematic_series_df$LB_Prob[i], "(*)"), problematic_series_df$LB_Prob[i])
    lb2_prob_display <- ifelse(problematic_series_df$LB2_Prob[i] < 0.05, paste0(problematic_series_df$LB2_Prob[i], "(*)"), problematic_series_df$LB2_Prob[i])
    norm_prob_display <- ifelse(problematic_series_df$Norm_Prob[i] < 0.05, paste0(problematic_series_df$Norm_Prob[i], "(*)"), problematic_series_df$Norm_Prob[i])
    
    cat(sprintf("%-5d %-5d %-15s %-8.2f %-10s %-10s %-10s %-10.2f %-25s\n",
                i,
                problematic_series_df$frequency[i],
                problematic_series_df$TITLE[i],
                problematic_series_df$BIC[i],
                lb_prob_display,
                lb2_prob_display,
                norm_prob_display,
                problematic_series_df$Norm_Test[i],
                problematic_series_df$Not_significant_coeff[i]),
        file = output_file, append = TRUE)
  }
  
  cat("\n", file = output_file, append = TRUE)
}


#' Create another Diagnostic Report for Time Series Models
#'
#' This function generates a diagnostic report for time series models.
#' It shows for every time series a complete view, including regression coefficients
#' with their T-statistics as well as LB, LB2, Normality tests and BIC 
#'
#' @param workspace A workspace object containing the time series models.
#' @param output_file A string specifying the output file path where the report will be saved (default is "report.txt").
#' @return NULL
#' @examples
#' require(RJDemetra)
#' 
#' original_directory <- getwd()
#' extdata_directory  <- system.file("extdata", package = "RJDProcessor")
#' setwd(extdata_directory)
#' ws_path <- "WorkspaceTUR-container/workspace-TUR.xml"
#' 
#' workspace <- load_workspace(file=ws_path)
#' create_diagnostic_report2(workspace, output_file = "report.out")
#' setwd(original_directory)
#' @export
create_diagnostic_report2 <- function(workspace, output_file="series_info.txt") {
  
  # if (!requireNamespace("RJDemetra", quietly = TRUE)) {
  #      stop("You need to install RJDemetra to use this function")
  # }else{require("RJDemetra")}
  
  if (file.exists(output_file)) {
    file.remove(output_file)
  }
  
  compute(workspace)
  jmodel <- get_jmodel(workspace)
  
  report_data <- list()
  idx <- 1
  for (multiprocessing in jmodel) {
    for (series_name in names(multiprocessing)) {
      time_series <- multiprocessing[[series_name]]
      
      ts <- get_indicators(x = time_series, "y")
      freq <- 12 / frequency(ts)
      
      bic        <- get_indicators(x = time_series, "preprocessing.likelihood.bic")[[1]]
      lb         <- get_indicators(x = time_series, "preprocessing.residuals.lb")[[1]]
      lb_squared <- get_indicators(x = time_series, "preprocessing.residuals.lb2")[[1]]
      normality  <- get_indicators(x = time_series, "preprocessing.residuals.dh")[[1]]
      
      model_descr  <- get_indicators(x = time_series, "preprocessing.model.description")[[1]]
      model_coeffs <- get_indicators(x = time_series, "preprocessing.model.coefficients")[[1]]
      model_coeffs_T_stat <- model_coeffs[, 1] / model_coeffs[, 2]
      
      cat("\n\n", file = output_file, append = TRUE)
      cat(sprintf("TITLE: %s\n", series_name), file = output_file, append = TRUE)
      
      cat(sprintf("%-15s %-15s %-15s %-15s %-15s\n", "BIC", "LB_Prob", "LB2_Prob", "Norm_Prob", "Norm_Test"), file = output_file, append = TRUE)
      cat("-----------------------------------------------------------------------------------------\n", file = output_file, append = TRUE)
      cat(sprintf("%-15.2f %-15.3f %-15.3f %-15.3f %-15.2f\n",
                  bic,
                  lb[2],
                  lb_squared[2],
                  normality[2],
                  normality[1]),
          file = output_file, append = TRUE)
      
      cat(sprintf("\n%-25s %-15s %-15s %-15s\n", "Regressor", "Coefficients", "T-Stats", "Not significant"), file = output_file, append = TRUE)
      cat("-------------------------------------------------------------------------------\n", file = output_file, append = TRUE)
      
      
      for (i in 1:length(model_descr)) {
        not_significant <- ifelse(abs(model_coeffs_T_stat[i]) < 2, "X", "")
        
        cat(sprintf("%-25s %-15.3f %-15.3f %-15s\n", model_descr[i], model_coeffs[i], model_coeffs_T_stat[i], not_significant),
            file = output_file, append = TRUE)
      }
    }
  }
}



