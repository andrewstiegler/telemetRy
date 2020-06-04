#' Calculate average parameters during light or dark cycle.
#'
#' @description
#' This function takes the output of DSI_export_to_dataframe and calculates
#' average telemetry parameters during the entire dark or light cycle.
#' @param data A dataframe which is the output from DSI_export_to_dataframe.
#' @param lights_on Time when the lights turn on. (24H)
#' @return A list containing two dataframes, one for the averages when lights
#' are off, and one for averages when lights are on. The first list element
#' is "Dark Averages" when room lights are OFF, the second list element is
#' "Light Averages" when room lights are ON.
#' @examples
#' circadian_avg(data = sample_BP_data, lights_on = 6)

# calculate average parameters in dark or light conditions ####
# circadian_avg function takes imported dataframe and lights_on time as input
# returns list of 2 elements - dark parameters, and light parameters

circadian_avg <- function (data, lights_on) {
    # Check inputs
    df_check <- is.data.frame(data)
    if (!df_check) stop("'data' must be dataframe.")
    lights_on_check <- missing(lights_on)
    if (lights_on_check) {
        lights_on <- 6
        print("'lights_on' not provided. Setting to default 6")
    }

    if (sum(grepl(".id", colnames(data))) != 0) {
        data_idcol <- (data %>% select(.data$.id) %>% unique())[[1]]
    } else data_idcol <- 1
    data_length <- length(data_idcol %>% unique())
    SNs <- data_idcol

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

    lights_on <- 3600 * lights_on
    LightsOff <- 12 * 3600 + lights_on

    if (LightsOff > 86400) {
        LightsOff <- LightsOff - 86400
    }

    if (lights_on < LightsOff) {
        Lights_zero <- "Off"
    } else (Lights_zero <- "On")

    data$Lights <- NA
    if (Lights_zero == "On") {
        data <- data %>% mutate(
            Lights = case_when(
                TimesOnly >= 0 & TimesOnly < LightsOff ~ "On",
                TimesOnly >= LightsOff & TimesOnly < lights_on ~ "Off",
                TimesOnly >= lights_on ~ "Off"
            )
        )
    }

    if (Lights_zero == "Off") {
        data <- data %>% mutate(
            Lights = case_when(
                TimesOnly >= 0 & TimesOnly < lights_on ~ "Off",
                TimesOnly >= lights_on & TimesOnly < LightsOff ~ "On",
                TimesOnly >= LightsOff ~ "Off"
            )
        )
    }

    data_dark_list <- list()
    data_light_list <- list()

    data_dark <- filter(data, .data$Lights == "Off")
    data_light <- filter(data, .data$Lights == "On")

    data_days <- ceiling(max(data$ElapsedTime)/86400)
    data_dark_avg <- data_light_avg <- data %>%
        select(-.data$Time, -.data$Lights) %>%
        head(data_days)

    data_dark_avg$TimesOnly <- data_light_avg$TimesOnly <- as.numeric(data_dark_avg$TimesOnly)

    for (days_iterator in 1:data_days) {
        data_dark_avg[days_iterator,] <- as.list(data_dark %>%
                                                     filter(.data$ElapsedTime/86400 < days_iterator &
                                                                .data$ElapsedTime/86400 >= days_iterator - 1) %>%
                                                     select(-.data$Time, -.data$Lights) %>%
                                                     colMeans(na.rm=TRUE))

        data_light_avg[days_iterator,] <- as.list(data_light %>%
                                                      filter(.data$ElapsedTime/86400 < days_iterator &
                                                                 .data$ElapsedTime/86400 >= days_iterator - 1) %>%
                                                      select(-.data$Time, -.data$Lights) %>%
                                                      colMeans(na.rm=TRUE))
        days_iterator <- days_iterator + 1
    }


    data_dark_all <- data_dark_avg
    data_light_all <- data_light_avg
    data_dark_all <- data_dark_all %>% select(-.data$ElapsedTime)
    data_light_all <- data_light_all %>% select(-.data$ElapsedTime)
    colnames(data_dark_all)[1] <- "Time"
    colnames(data_light_all)[1] <- "Time"
    data_dark_all$Time <- c(1:data_days)
    data_light_all$Time <- c(1:data_days)

    circadian_list <- list(data_dark_all, data_light_all)
    names(circadian_list)[1:2] <- c("Dark Averages", "Light Averages")
    return (circadian_list)
}
