#' Calculate average parameters for each timepoint in a circadian cycle.
#'
#' This function calculates the typical parameters in a dataframe for one
#' circadian cycle. Use this function on a dataframe generated from a DSI export
#'  using the DSI_export_to_dataframe function.
#' @describeIn typical_day Calculate typical day from telemetry data.
#' @param data A dataframe created using the DSI_export_to_dataframe function.
#' @param lights_on Time when the lights turn on. (24H)
#' @param progressbar Boolean for whether to show progress of calculation.
#' @param avg_minutes Number of minutes to average over (integer)
#' @return A dataframe containing a column of circadian time and parameters.
#' @examples
#' typical_day(data = sample_BP_data, lights_on = 21)
#' typical_day(data = sample_BP_data, lights_on = 6)
#' @import dplyr
#' @import magrittr
#' @importFrom data.table as.ITime

typical_day <- function(data, lights_on, progressbar = FALSE) {

    df_check <- is.data.frame(data)
    if (!df_check) return("'data' must be dataframe.")

    df_classes <- sapply(data, class)

    ITime_check <- sum(df_classes == "ITime")
    if (ITime_check == 0) {
        df_is_posixt <- sapply(data, is.POSIXct)
        if (sum(sapply(data, is.POSIXct)) == 0)
            {return("Must contain a column of ITime or POSIXct")
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

# initialize list of typical days for filling in
typical_list <- list()

# loops over SNs
for (SN_iterator in 1:length(data$.id %>% unique())) {
    if(progressbar) {
    progress(SN_iterator, length(data$.id %>% unique()))}
    # loops over times
    times_iterator <- 1

    single_SN_data <- filter(data, .data$.id ==
                                 (data$.id %>% unique())[SN_iterator])

    typical_day <- tibble(Time = seq(from = t_begin, to = (t_begin+86390),
                                     by = t_delta),
                          SBP = c(1:total_times),
                          DBP = c(1:total_times),MAP = c(1:total_times),
                          HR = c(1:total_times),Temp = c(1:total_times),
                          Activity = c(1:total_times))

    time_column <- typical_day$Time

    while (times_iterator <= total_times) {
        single_SN_single_time <- filter(single_SN_data, .data$TimesOnly ==
                                            time_column[times_iterator]) %>%
            select( -.data$.id, -.data$Time, -.data$TimesOnly) %>%
            colMeans(na.rm = TRUE)
        typical_day$SBP[times_iterator] <- single_SN_single_time[5]
        typical_day$DBP[times_iterator] <- single_SN_single_time[6]
        typical_day$MAP[times_iterator] <- single_SN_single_time[7]
        typical_day$HR[times_iterator] <- single_SN_single_time[8]
        typical_day$Temp[times_iterator] <- single_SN_single_time[9]
        typical_day$Activity[times_iterator] <- single_SN_single_time[10]
        if(times_iterator == total_times){
            colnames(typical_day)[2] <- paste(unique(data$.id)[SN_iterator],
                                              "SBP",sep="")
            colnames(typical_day)[3] <- paste(unique(data$.id)[SN_iterator],
                                              "DBP",sep="")
            colnames(typical_day)[4] <- paste(unique(data$.id)[SN_iterator],
                                              "MAP",sep="")
            colnames(typical_day)[5] <- paste(unique(data$.id)[SN_iterator],
                                              "HR",sep="")
            colnames(typical_day)[6] <- paste(unique(data$.id)[SN_iterator],
                                              "Temp",sep="")
            colnames(typical_day)[7] <- paste(unique(data$.id)[SN_iterator],
                                              "Activity",sep="")
            typical_list[[SN_iterator]] <- typical_day
                names(typical_list)[[SN_iterator]] <-
                    paste(unique(data$.id)[SN_iterator])
            }
        times_iterator<-times_iterator+1
        }
        SN_iterator <- SN_iterator + 1
    }

#Group all SNs into one tibble for output
all_typical <- typical_list %>% reduce(left_join, by = "Time")

# Add columns for status of room lights ----
lights_iterator <- 2
lights_initial <- as.numeric(data.table::as.ITime(lights_on*3600) - all_typical$Time[1])
if (lights_initial<0) {lights_initial <- lights_initial*-1}
all_typical$LightsTime <- lights_initial
while (lights_iterator < length(all_typical$Time)) {
    if (as.numeric(all_typical$Time[lights_iterator] -
                   all_typical$Time[lights_iterator-1]) >= 0) {
        LightsNext <- as.numeric(all_typical$Time[lights_iterator] -
                                     all_typical$Time[lights_iterator-1])
    }
    if (as.numeric(all_typical$Time[lights_iterator] -
                   all_typical$Time[lights_iterator-1])<0){
        LightsNext <- as.numeric(all_typical$Time[lights_iterator] -
                                     all_typical$Time[lights_iterator-1])+86400
    }
    if (lights_initial+LightsNext==86400){lights_initial<-lights_initial-86400}
    all_typical$LightsTime[lights_iterator]<-lights_initial+LightsNext
    lights_iterator <- lights_iterator+1
    lights_initial <- lights_initial+LightsNext
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
# return ----
return (all_typical)
}

#' @describeIn typical_day Average typical_day over specified time interval.

# Arbitrary average of typical_day data
typical_average <- function(data, avg_minutes) {

    seconds_per_row <- data$LightsTime[2] - data$LightsTime[1]
    rows_per_hour <- 60*avg_minutes / seconds_per_row
    data <- data %>% select(-.data$LightsOn)
    for (rows_avg_iterator in 1:nrow(data)/rows_per_hour) {
        data[rows_avg_iterator,] <- data %>%
            slice((rows_avg_iterator*rows_per_hour-(rows_per_hour-1)):
                      (rows_avg_iterator*rows_per_hour)) %>%
            colMeans(na.rm=TRUE) %>%
            stack() %>%
            as_tibble() %>%
            spread(key=.data$ind, value=.data$values)
        rows_avg_iterator <- rows_avg_iterator+1
    }

    data <- head(data,nrow(data)/rows_per_hour)

    data$LightsOn <- 0
    data <- data %>% mutate(
        LightsOn = case_when(
            LightsTime < 43200 ~ "Y",
            LightsTime >= 43200 ~ "N"
        )
    )

    return(data) }

