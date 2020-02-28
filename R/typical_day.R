#' Calculate average parameters for each timepoint in a circadian cycle.
#'
#' This function calculates the typical parameters in a dataframe for one
#' circadian cycle. Use this function on a dataframe generated from a DSI export
#'  using the DSI_export_to_dataframe function.
#' @param data A dataframe created using the DSI_export_to_dataframe function..
#' @param lights_on Time when the lights turn on. (24H, as integer)
#' @return A dataframe containing a column of circadian time and parameters.
#' @examples
#' typical_day(data = data, lights_on = 21)
#' typical_day(data = data, lights_on = 6)

typical_day <- function(data, lights_on) {

#Create 24-hour averages for baseline and hypertensive data for comparisons
t_begin <- data$TimesOnly[1]
t_next <- data$TimesOnly[2]
t_delta <- t_next-t_begin
total_times <- as.numeric(86400 / t_delta)

# initialize blank typical day for filling in


# initialize list of typical days for filling in
typical_list <- list()

# loops over SNs
SN_iterator <- 1
while (SN_iterator <= length(data$.id %>% unique())) {
    # loops over times
    times_iterator <- 1

    single_SN_data <- filter(data, .id == (data$.id %>% unique())[SN_iterator])

    typical_day <- tibble(Time = seq(from = t_begin, to = (t_begin+86390),
                                     by = t_delta),
                          SBP = c(1:total_times),
                          DBP = c(1:total_times),MAP = c(1:total_times),
                          HR = c(1:total_times),Temp = c(1:total_times),
                          Activity = c(1:total_times))

    time_column <- typical_day$Time

    while (times_iterator <= total_times) {
        single_SN_single_time <- filter(single_SN_data, TimesOnly ==
                                            time_column[times_iterator]) %>%
            select( -.id, -Time, -TimesOnly) %>% colMeans(na.rm = TRUE)
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
lights_initial <- as.numeric(as.ITime(lights_on*3600) - all_typical$Time[1])
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
        LightsNext <- as.numeric(all_typical$Time[lights_iterator]-all_typical$Time[lights_iterator-1])+86400
    }
    if (lights_initial+LightsNext==86400){lights_initial<-lights_initial-86400}
    all_typical$LightsTime[lights_iterator]<-lights_initial+LightsNext
    lights_iterator <- lights_iterator+1
    lights_initial <- lights_initial+LightsNext
}
all_typical$LightsTime[length(all_typical$Time)] <-
    lights_initial+as.numeric(all_typical$Time[length(all_typical$Time)]
                              -all_typical$Time[length(all_typical$Time)-1])

all_typical <- all_typical %>% mutate(
    LightsOn = case_when(
        LightsTime < 43200 ~ "Y",
        LightsTime >= 43200 ~ "N"
    )
)
# return ----
return (all_typical)
}

# Fit circadian cycle to cosine wave
typical_average <- function(data, avg_minutes) {

    rows_avg_iterator <- 1
    seconds_per_row <- data$LightsTime[2] - data$LightsTime[1]
    rows_per_hour <- 60*avg_minutes / seconds_per_row
    data <- data %>% select(-LightsOn)
    while (rows_avg_iterator<=nrow(data)/rows_per_hour) {
        data[rows_avg_iterator,] <- data %>%
            slice((rows_avg_iterator*rows_per_hour-(rows_per_hour-1)):
                      (rows_avg_iterator*rows_per_hour)) %>%
            colMeans(na.rm=TRUE) %>%
            stack() %>%
            as_tibble() %>%
            spread(key=ind, value=values)
        rows_avg_iterator <- rows_avg_iterator+1
    }

    data <- head(data,nrow(data)/rows_per_hour)

    data <- data %>% mutate(
        LightsOn = case_when(
            LightsTime < 43200 ~ "Y",
            LightsTime >= 43200 ~ "N"
        )
    )

    return(data) }



