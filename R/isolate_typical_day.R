#' Isolate parameters from typical_day dataframe.
#'
#' @description
#' These functions take the output of typical_day and outputs a new
#' dataframe for the specified parameter.
#' @describeIn typical_sbp
#' @param data A dataframe which is the output from typical_day.
#' @return A dataframe with the Time and elapsed_time columns from the input,
#' and a column for each subject appended with the parameter (i.e. SBP).
#' Additionally a column for the mean of all subjects in the dataset.
#' @examples
#' typical_sbp(typical_day_output)
#' typical_dbp(typical_day_output)
#' typical_map(typical_day_output)
#' typical_temp(typical_day_output)
#' typical_HR(typical_day_output)
#' typical_activity(typical_day_output)
#' @describeIn typical_sbp Isolate SBP from typical_day dataframe.

# isolate typical parameters from typical day dataset
typical_sbp <- function (data) {
    sbp_typical <- select(data, 1, grep("SBP", colnames(data)))
    return (sbp_typical)}

#' @describeIn typical_sbp Isolate DBP from typical_day dataframe.
typical_dbp <- function (data) {
    dbp_typical <- select(data, 1, grep("Sys", colnames(data)))
    return (dbp_typical)}

#' @describeIn typical_sbp Isolate MAP from typical_day dataframe.
typical_map <- function (data) {
    map_typical <- select(data, 1, grep("MAP", colnames(data)))
    return (map_typical)}

#' @describeIn typical_sbp Isolate temperature from typical_day dataframe.
typical_temp <- function (data) {
    temp_typical <- select(data, 1, grep("Temp", colnames(data)))
    return (temp_typical)}

#' @describeIn typical_sbp Isolate HR from typical_day dataframe.
typical_HR <- function (data) {
    HR_typical <- select(data, 1, grep("HR", colnames(data)))
    return (HR_typical)}

#' @describeIn typical_sbp Isolate activity from typical_day dataframe.
typical_activity <- function (data) {
    activity_typical <- select(data, 1, grep("Activity", colnames(data)))
    return (typical_activity)}
