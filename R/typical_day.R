# Typical day calculates BP parameters over a single circadian cycle
# Description

typical_day <- function(data, lights_on) {

#Create 24-hour averages for baseline and hypertensive data for comparisons
t_begin <- data$TimesOnly[1]
t_next <- data$TimesOnly[2]
t_delta <- t_next-t_begin
total_times <- as.numeric(86400 / t_delta)

# initialize blank typical day for filling in
typical_day <- tibble(Time = seq(from = t_begin, to = (t_begin+86390),
                                 by = t_delta),
                      SBP = c(1:total_times),
                      DBP = c(1:total_times),MAP = c(1:total_times),
                      HR = c(1:total_times),Temp = c(1:total_times),
                      Activity = c(1:total_times))

time_column <- typical_day$Time

# initialize list of typical days for filling in
typical_list <- list()

# loops over SNs
SN_iterator <- 1
while (SN_iterator <= length(data$.id %>% unique())) {

    # loops over times
    times_iterator <- 1
    single_SN_data <- filter(data, .id == (data$.id %>% unique())[SN_iterator])
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
lights_on <- 3600 * lights_on
lights_off <- 12 * 3600 + lights_on
if (lights_off > 86400) {
    lights_off <- lights_off - 86400
}

if (lights_on < lights_off) {
    Lights_zero <- "Off"
} else (Lights_zero <- "On")

if (Lights_zero == "On") {
    all_typical <- all_typical %>% mutate(
        Lights = case_when(
            Time >= 0 & Time < lights_off ~ "On",
            Time >= lights_off & Time < lights_on ~ "Off",
            Time >= lights_on ~ "Off"
        )
    )
}

if (Lights_zero == "Off") {
    all_typical <- all_typical %>% mutate(
        Lights = case_when(
            Time >= 0 & Time < lights_on ~ "Off",
            Time >= lights_on & Time < lights_off ~ "On",
            Time >= lights_off ~ "Off"
        )
    )
}
# return ----
return (all_typical)
}

# Fit circadian cycle to cosine wave
typical_average <- function(data, avg_minutes) {

    rows_avg_iterator <- 1
    seconds_per_row <- data$Time[2] - data$Time[1]
    rows_per_hour <- 60*avg_minutes / seconds_per_row

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
    return(data) }


