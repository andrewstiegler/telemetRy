#' Isolate parameters from complete telemetry dataframe.
#'
#' @description
#' These functions take the output of DSI_export_to_dataframe and outputs a new
#' dataframe for the specified parameter.
#' @param data A dataframe which is the output from DSI_export_to_dataframe.
#' @param parameter Name of a parameter to isolate in the dataframe.
#' @param mean_col A boolean for including a new column with the group mean for
#' the selected parameter.
#' @return A dataframe with the Time and elapsed_time columns from the input,
#' and a column for each subject appended with the parameter (i.e. SBP).
#' Additionally an optional column for the mean of all subjects in the dataset.
#' @examples
#' isolate_parameter(sample_BP_data, SBP, mean_col = TRUE)
#' isolate_parameter(sample_BP_data, MAP)
#' @describeIn isolate_parameter Isolate parameter from complete telemetry
#' dataframe export.
#'
# isolate_sbp function takes DSI_export_to_dataframe output and isolates SBP
# keeps time columns, each SN gets an SBP column named "SNSBP"

isolate_parameter <- function (data, parameter, mean_col=FALSE) {

    quo_parameter <- enquo(parameter)

    if (sum(grepl(".id", colnames(data))) != 0) {
        data_idcol <- data$.id %>% unique()
    } else data_idcol <- 1
    data_idcol <- (data %>% select(.data$.id) %>% unique())[[1]]

    parameter_joined <- list()

    for (isolate_iterator in 1:length(data_idcol)){
        parameter_next <- filter(data, .data$.id ==
                                 data_idcol[[isolate_iterator]]) %>%
            select(.data$Time, !! quo_parameter)
        colnames(parameter_next)[2] <- paste(data_idcol[[isolate_iterator]],
                                             quo_get_expr(enquo(parameter)),
                                             sep="")
        parameter_joined[[isolate_iterator]] <- parameter_next
        isolate_iterator <- isolate_iterator + 1
    }

    parameter_joined_full <- parameter_joined %>% reduce(left_join, by="Time")
    if (mean_col == TRUE) {
        parameter_joined_full$mean <-
            rowMeans(parameter_joined_full %>% select(-1), na.rm= TRUE)
    }

    parameter_joined_full$elapsed_time[1] <- 0

    for (elapsed_iterator in 2:length(parameter_joined_full$Time)){
        parameter_joined_full$elapsed_time[elapsed_iterator] =
            as.numeric(parameter_joined_full$Time[elapsed_iterator]) -
            as.numeric(parameter_joined_full$Time[1])
        elapsed_iterator <- elapsed_iterator+1
    }

    return (parameter_joined_full)
}
