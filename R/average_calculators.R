# hourly_average function takes dataframe of isolated parameters as input
# returns hourly average of parameter and keeps time columns
hourly_average <- function(avg_data_tibble) {

    rows_avg_iterator <- 1
    seconds_per_row <- avg_data_tibble$elapsed_time[2] - avg_data_tibble$elapsed_time[1]
    rows_per_hour <- 3600 / seconds_per_row
    avg_data_tibble_notime <- avg_data_tibble %>% select(-1)
    while (rows_avg_iterator<=nrow(avg_data_tibble)/rows_per_hour) {
        avg_data_tibble_notime[rows_avg_iterator,] <- avg_data_tibble_notime %>%
            slice((rows_avg_iterator*rows_per_hour-(rows_per_hour-1)):(rows_avg_iterator*rows_per_hour)) %>%
            colMeans(na.rm=TRUE) %>%
            stack() %>%
            as_tibble() %>%
            spread(key=.data$ind, value=.data$values)
        rows_avg_iterator <- rows_avg_iterator+1
    }

    avg_data_tibble_notime <- head(avg_data_tibble_notime,nrow(avg_data_tibble_notime)/rows_per_hour)
    avg_data_tibble_notime$Time <- avg_data_tibble$Time[1] + avg_data_tibble_notime$elapsed_time
    avg_data_tibble_notime <- avg_data_tibble_notime %>%
        select(.data$Time, .data$elapsed_time, everything())
    return(avg_data_tibble_notime) }

# arb_average function averages post-inj parameter isolated data
arb_average <- function(data, avg_minutes) {

    rows_avg_iterator <- 1
    seconds_per_row <- data$InjTime[2] - data$InjTime[1]
    rows_per_hour <- 60*avg_minutes / seconds_per_row

    while (rows_avg_iterator<=nrow(data)/rows_per_hour) {
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
    return(data) }
