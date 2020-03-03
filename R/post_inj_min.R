#' Calculate the minimum value of parameters after injection.
#'
#' This function calculates the minimum value of telemetry parameters after
#' an injection. The input for this function is the output of the post_inj
#' functions, i.e. post_inj_sbp.
#'
#' @param data A dataframe created using the DSI_export_to_dataframe function.
#' @param duration Maximum time after injection to look for minimum (hours).
#' @return A dataframe containing the minimum value  of the parameter for each
#' SN.
# i.e. output of post_inj_sbp
# caclulates minimum 5-minute average after injection (200s to 6 hrs post-inj)
#' @import dplyr
#' @import magrittr
#' @importFrom Rfast colMins
#' @export post_inj_min

post_inj_min <- function (data, duration) {

    SNs <- colnames(select(data, -.data$mean, -.data$se, -.data$InjTime,
                           -.data$InjTimeH))
    # Data sets are 10 s/row, so to average over 5 min
    rows_to_avg_num <- 30

    # Set how many hours post-injection to look for minima
    hours_post <- duration
    second_offset <- 200
    data_for_min_data_trim <- data %>%
        filter(.data$InjTime > second_offset &
                   .data$InjTime <= 3600*hours_post) %>%
        select(-.data$mean, -.data$se, -.data$InjTime, -.data$InjTimeH)

    avg_data_to_use_data <- data_for_min_data_trim
    rows_avg_iterator <- 1
    avg_data_tibble <- avg_data_to_use_data
    while (rows_avg_iterator <= nrow(avg_data_to_use_data) / rows_to_avg_num) {
        avg_data_tibble[rows_avg_iterator,] <- avg_data_to_use_data %>%
            slice((rows_avg_iterator*rows_to_avg_num - (rows_to_avg_num-1)):
                      (rows_avg_iterator*rows_to_avg_num)) %>%
            colMeans(na.rm=TRUE) %>%
            stack() %>%
            as_tibble() %>%
            spread(key=.data$ind,value=.data$values)

        rows_avg_iterator <- rows_avg_iterator + 1
    }
    data_for_min_data_avg <- head(avg_data_tibble,nrow(avg_data_to_use_data)/
                                      rows_to_avg_num) %>% as.matrix()

    min_delta_values <- colMins(data_for_min_data_avg, value=TRUE)
    min_delta_times <- colMins(data_for_min_data_avg, value=FALSE)
    min_delta_times <- min_delta_times*
        rows_to_avg_num*10+10+second_offset

    min_tibble <- tibble(SNs = SNs, min = min_delta_values)
    min_tibble <- spread(min_tibble, SNs, min)
    return (min_tibble)
}
