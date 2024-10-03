#' RJDProcessor
#'
#' The RJDProcessor library integrates the rjdverse packages into a fully R-based production pipeline, ready to be used and easily extendable by methodologists.
#' It offers the capability to manage the entire seasonal adjustment process: acquisition, processing, storage, automation, and not just seasonal adjustment of the data.
#' Processing of multiple time series is possible by storing their specifications in JSON files, and interoperability with other JDemetra+ software is guaranteed because RJDProcessor can read workspaces and is able to produce them as an output.
#' RJDProcessor also provides functions to manage workspaces, such as splitting a workspace containing multiple time series into individual single-series workspaces, which are suitable for storing in databases with single time series records. Functions to merge workspaces are also available.
#'
#' @docType package
#' @name RJDProcessor
#' @title RJDProcessor
#' @author Alessandro Piovani <alessandro.piovani@istat.it>, <alessandro.piovani13@gmail.com>;
#' @description A fully RJDemetra-based production pipeline for official statistics.
#'
#'
NULL
