#' Average telemetry datasets over arbitrary times..
#'
#' This function calculates the average of a parameter over a specified time
#' period.
#' @describeIn arb_time_average Add injection time column to dataset.
#' @param data A dataframe created using the DSI_export_to_dataframe function.
#' @param avg_minutes The number of minutes to average over.
#' @return A dataframe containing the numeric columns of the input averaged
#' over the specified time interval.
#' @examples
#' arb_time_average(sample_BP_data, avg_minutes = 20)


# arb_average function averages post-inj parameter isolated data
arb_time_average <- function(data, avg_minutes) {

    avg_minutes_missing <- missing(avg_minutes)
    if (avg_minutes_missing) {
        stop("Missing avg_minutes.")
    }

    df_check <- is.data.frame(data)
    if (!df_check) stop("'data' must be dataframe.")

    df_classes <- sapply(data, class)
    ITime_check <- sum(df_classes == "ITime")
    df_is_posixt <- sapply(data, is.POSIXct)

    if (ITime_check == 0) {
        if (sum(sapply(data, is.POSIXct)) == 0){
            stop("Must contain a column of ITime or POSIXct")
        }
    }

    if (ITime_check > 1) {
        warning("More than one ITime column. Selected first ITime column.")

    }

    if (sum(sapply(data, is.POSIXct)) > 1){
        warning("More than one POSIXct column. Selected first POSIXct column.")
    }

    if (ITime_check == 1 & sum(sapply(data, is.POSIXct)) == 0) {
        time_delta <- as.numeric((data %>%
                                select(which(df_classes == "ITime")[1]))[2] -
                                (data %>%
                                select(which(df_classes == "ITime")[1]))[1])
    }

    if (ITime_check == 0 & sum(sapply(data, is.POSIXct)) == 1) {
        time_delta <- as.numeric((data %>%
                                select(which(df_is_posixt)[1]))[2] -
                                (data %>%
                                select(which(df_is_posixt)[1]))[1])
    }

    if (ITime_check == 1 & sum(sapply(data, is.POSIXct)) == 1) {
        time_delta <- as.numeric((data %>%
                                      select(which(df_is_posixt)[1]))[2,] -
                                     (data %>%
                                          select(which(df_is_posixt)[1]))[1,])
    }


    rows_per_avg <- 60 * (avg_minutes / time_delta)

    if (sum(sapply(data, is.POSIXct)) > 0) {
        data <- data %>% select(-as.numeric(which(df_is_posixt))[1])
    }

    for (rows_avg_iterator in 1:(nrow(data)/rows_per_avg)) {
        data[rows_avg_iterator,] <- data %>%
            slice((rows_avg_iterator*rows_per_avg-(rows_per_avg-1)):
                      (rows_avg_iterator*rows_per_avg)) %>%
            colMeans(na.rm=TRUE) %>%
            stack() %>%
            as_tibble() %>%
            spread(key=.data$ind, value=.data$values)
        rows_avg_iterator <- rows_avg_iterator+1
    }

    for (rows_avg_iterator in 1:(nrow(data)/rows_per_avg)) {
        time_only <- data %>% select(as.numeric(which(df_is_posixt))[1])
        time_only <- time_only %>%
            slice((rows_avg_iterator*rows_per_avg-(rows_per_avg-1)):
                            (rows_avg_iterator*rows_per_avg)) %>%
            colMeans(na.rm=TRUE)
    }

    data <- head(data, nrow(data)/rows_per_avg)
    return(data)
}
