#' Calculate average parameters for each timepoint in a circadian cycle.
#'
#' This function calculates the typical parameters in a dataframe for one
#' circadian cycle. Use this function on a dataframe generated from a DSI export
#'  using the DSI_export_to_dataframe function.
#' @describeIn typical_day Calculate typical day from telemetry data.
#' @param data A dataframe created using the DSI_export_to_dataframe function.
#' @param lights_on Time when the lights turn on. (24H)
#' @param include_lights A boolean for including columns showing status of room
#' lights. FALSE by default. If TRUE, output includes a column for light status
#' ("LightsOn") and a column for time since lights turned on ("LightsTime")
#' @param avg_minutes Number of minutes to average over (integer)
#' @return A dataframe containing a column of circadian time and parameters.
#' @examples
#' typical_day(data = sample_BP_data, lights_on = 21)
#' typical_day(data = sample_BP_data, lights_on = 6)
#' @import dplyr
#' @import magrittr
#' @importFrom data.table as.ITime

typical_day <- function(data, lights_on, include_lights = FALSE) {

    df_check <- is.data.frame(data)
    if (!df_check) stop("'data' must be dataframe.")

    idcol_check <- sum(grep(".id", colnames(data)))
    if (idcol_check == 1) {

        data_list_wide <- list()
        for (wide_iterator in 1:length(data$.id %>% unique())){
            data_list_wide[[wide_iterator]] <-
                filter(data, .data$.id ==
                           (data$.id %>% unique())[[wide_iterator]])
            colnames(data_list_wide[[wide_iterator]]) <-
                paste((data$.id %>% unique())[[wide_iterator]],
                      colnames(data_list_wide[[wide_iterator]]), sep = "_")
            colnames(data_list_wide[[wide_iterator]])[2] <-
                c("Time")
            wide_iterator <- wide_iterator + 1
        }

        all_data_wide <- data_list_wide %>% reduce(left_join, by = "Time")
        colnames(all_data_wide)[3:4] <- c("OnlyTime", "TimeElapsed")
        all_data_wide <- all_data_wide %>%
            select(-grep("TimesOnly", colnames(all_data_wide)),
                   -grep("ElapsedTime", colnames(all_data_wide)),
                   -grep(".id", colnames(all_data_wide)))
        colnames(all_data_wide)[2:3] <- c("TimesOnly", "ElapsedTime")
        data <- all_data_wide
    }


    df_classes <- sapply(data, class)
    ITime_check <- sum(df_classes == "ITime")
    if (ITime_check == 0) {
        df_is_posixt <- sapply(data, is.POSIXct)
        if (sum(sapply(data, is.POSIXct)) == 0){
            stop("Must contain a column of ITime or POSIXct")
        }
        colnames(data)[which(df_is_posixt)] <- "Time"
        data$TimesOnly <- data.table::as.ITime(data$Time)
    }

    lights_on_check <- missing(lights_on)
    if (lights_on_check) {
        lights_on <- 6
        print("'lights_on' not provided. Setting to default 6")
    }
    #Create 24-hour averages for baseline and hypertensive data for comparisons
    t_begin <- data$TimesOnly[1]
    t_next <- data$TimesOnly[2]
    t_delta <- t_next-t_begin
    total_times <- as.numeric(86400 / t_delta)

    time_column <- seq(from = t_begin, to = t_begin+86400-t_delta, by = t_delta)
    data_time_column <- which(sapply(data, is.POSIXct))

    typical_day <- data %>% select(-.data$Time) %>% head(total_times)

    for (times_iterator in 1:total_times) {
        single_time <- data %>% filter(.data$TimesOnly ==
                                           time_column[times_iterator]) %>%
            select(-data_time_column) %>%
            colMeans(na.rm = TRUE)
        typical_day[times_iterator,] <- single_time
        times_iterator<-times_iterator+1
    }

    if (sum(grep("ElapsedTime", colnames(typical_day))) != 0){
        all_typical <- typical_day %>% select(-.data$ElapsedTime)
    } else all_typical <- typical_day

    colnames(all_typical)[which(colnames(all_typical) == "TimesOnly")] <- "Time"

    if (include_lights == "TRUE") {
    # Add columns for status of room lights ----
        lights_iterator <- 2
        lights_initial <- as.numeric(as.ITime(lights_on*3600) -
                                         all_typical$Time[1])
        if (lights_initial<0) {lights_initial <- lights_initial*-1}
            all_typical$LightsTime <- lights_initial

        while (lights_iterator < length(all_typical$Time)) {
            if (as.numeric(all_typical[lights_iterator, data_time_column] -
                       all_typical[lights_iterator - 1, data_time_column]) >=
                       0){
                LightsNext <-
                as.numeric(all_typical[lights_iterator, data_time_column] -
                           all_typical[lights_iterator-1, data_time_column])
            }

            if (as.numeric(all_typical[lights_iterator, data_time_column] -
                       all_typical[lights_iterator-1, data_time_column]) < 0){
                LightsNext <-
                as.numeric(all_typical[lights_iterator, data_time_column] -
                           all_typical[lights_iterator-1, data_time_column]) +
                           86400
            }

            if (lights_initial + LightsNext == 86400) {
                lights_initial <- lights_initial-86400
            }

            all_typical$LightsTime[lights_iterator]<-lights_initial + LightsNext
            lights_iterator <- lights_iterator+1
            lights_initial <- lights_initial + LightsNext
        }

        all_typical$LightsTime[length(all_typical$Time)] <-
            lights_initial+as.numeric(all_typical$Time[length(all_typical$Time)]
                                  -all_typical$Time[length(all_typical$Time)-1])

        all_typical$LightsOn <- 0
        all_typical <- all_typical %>% mutate(
            LightsOn = case_when(
                LightsTime < 43200 ~ "Y",
                LightsTime >= 43200 ~ "N"
                )
            )
    }
    # return ----
    return (all_typical)
}

#' @describeIn typical_day Average typical_day over specified time interval.

# Arbitrary average of typical_day data
typical_average <- function(data, avg_minutes) {

    avg_minutes_missing <- missing(avg_minutes)
    if (avg_minutes_missing) {
        stop("avg_minutes not supplied. Enter number of minutes to average.")
    }
    seconds_per_row <- data$LightsTime[2] - data$LightsTime[1]
    rows_per_avg <- 60*avg_minutes / seconds_per_row
    data <- data %>% select(-.data$LightsOn)
    for (rows_avg_iterator in 1:(nrow(data)/rows_per_avg)) {
        data[rows_avg_iterator,] <- data %>%
            slice((1+rows_per_avg * (rows_avg_iterator - 1)):
                      (rows_avg_iterator*rows_per_avg)) %>%
            colMeans(na.rm=TRUE) %>%
            stack() %>%
            as_tibble() %>%
            spread(key=.data$ind, value=.data$values)
        rows_avg_iterator <- rows_avg_iterator+1
    }

    data <- head(data,nrow(data)/rows_per_avg)

    data$LightsOn <- 0
    data <- data %>% mutate(
        LightsOn = case_when(
            LightsTime < 43200 ~ "Y",
            LightsTime >= 43200 ~ "N"
        )
    )

    return(data) }

