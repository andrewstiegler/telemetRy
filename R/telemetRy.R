#' telemetRy: A package for interacting with telemetry data in R.
#'
#' This is a package for interacting with telemetry data using R. Specific
#' functions for importing data from DSI telemetry systems are included, as well
#' as functions for analyzing telemetry data over various timescales.
#'
#' @section Functions:
#' DSI_export_to_dataframe
#' A simple function for bringing exports from DSI's Ponemah software into R.
#' Takes an Excel file as its input, and returns a dataframe.
#'
#' typical_day
#' This function takes a dataframe with a time column as input. The function
#' generates a "typical day" dataframe, with a time column with the temporal
#' resolution of the input, and calculates the average of multiple
#' observations at each timepoint.
#'
#' circadian_avg
#' This function takes a dataframe with multiple circadian cycles as input, and
#' outputs a dataframe averaging each column over the dark or light circadian
#' cycle.
#' @docType package
#' @name telemetRy
#'
#' @import readxl
#' @import dplyr
#' @import magrittr
#' @importFrom data.table as.ITime
#' @importFrom data.table rbindlist
#' @importFrom svMisc progress
#' @importFrom janitor excel_numeric_to_date
#' @importFrom purrr reduce
#' @importFrom lubridate is.POSIXct
#'
#' @export DSI_export_to_dataframe
#' @export typical_day
#' @export typical_average
#' @export isolate_typical
#' @export add_injtime
#' @export isolate_postinj
#' @export circadian_avg
NULL
