#' Calculate average parameters during light or dark cycle.
#'
#' @description
#' Theis function takes the output of DSI_export_to_dataframe and calculates
#' average telemetry parameters during the entire dark or light cycle.
#' @param data A dataframe which is the output from DSI_export_to_dataframe.
#' @param lights_on Time when the lights turn on. (24H)
#' @return A list containing two dataframes, one for the averages when lights
#' are off, and one for averages when lights are on. The first list element
#' is "Dark Averages" when room lights are OFF, the second list element is
#' "Light Averages" when room lights are ON.
#' @examples
#' circadian_avg(data = data, lights_on = 6)

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

    data_length <- length(data$.id %>% unique())
    SNs <- data$.id %>% unique()

    lights_on <- 3600 * lights_on
    LightsOff <- 12 * 3600 + lights_on
    if (LightsOff > 86400) {
        LightsOff <- LightsOff - 86400
    }

    if (lights_on < LightsOff) {
        Lights_zero <- "Off"
    } else (Lights_zero <- "On")

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
    for (data_iterator in 1:data_length) {
        data_singleSN <- data %>% filter(.id == SNs[data_iterator])
        data_days <- ceiling(data_singleSN$ElapsedTimeD %>% max())
        data_dark <- filter(data_singleSN, Lights == "Off")
        data_light <- filter(data_singleSN, Lights == "On")

        data_dark_avg <- tibble(ElapsedTimeD = c(1:data_days),
                                SBP = c(1:data_days), DBP = c(1:data_days),
                                MAP = c(1:data_days), HR = c(1:data_days),
                                Temp = c(1:data_days),
                                Activity = c(1:data_days))
        data_light_avg <- tibble(ElapsedTimeD = c(1:data_days),
                                 SBP = c(1:data_days), DBP = c(1:data_days),
                                 MAP = c(1:data_days), HR = c(1:data_days),
                                 Temp = c(1:data_days),
                                 Activity = c(1:data_days))

        for (days_iterator in 1:data_days) {
            data_dark_avg[days_iterator,] <- data_dark %>%
                select(-1:-6,-Lights) %>%
                filter(ElapsedTimeD < days_iterator &
                           ElapsedTimeD >= days_iterator - 1) %>%
                colMeans(na.rm=TRUE)

            data_light_avg[days_iterator,] <- data_light %>%
                select(-1:-6,-Lights) %>%
                filter(ElapsedTimeD < days_iterator &
                           ElapsedTimeD >= days_iterator - 1) %>%
                colMeans(na.rm=TRUE)

            if (days_iterator == data_days) {
                colnames(data_dark_avg)[2:7] <-
                    c(paste(SNs[data_iterator],"SBP",sep=""),
                      paste(SNs[data_iterator],"DBP",sep=""),
                      paste(SNs[data_iterator],"MAP",sep=""),
                      paste(SNs[data_iterator],"Temp",sep=""),
                      paste(SNs[data_iterator],"HR",sep=""),
                      paste(SNs[data_iterator],"Activity",sep=""))
                data_dark_avg$ElapsedTimeD <- c(1:data_days)

                colnames(data_light_avg)[2:7] <-
                    c(paste(SNs[data_iterator],"SBP",sep=""),
                      paste(SNs[data_iterator],"DBP",sep=""),
                      paste(SNs[data_iterator],"MAP",sep=""),
                      paste(SNs[data_iterator],"Temp",sep=""),
                      paste(SNs[data_iterator],"HR",sep=""),
                      paste(SNs[data_iterator],
                            "Activity",sep=""))
                data_light_avg$ElapsedTimeD <- c(1:data_days)
                data_dark_list[[data_iterator]] <- data_dark_avg
                data_light_list[[data_iterator]] <- data_light_avg
            }
            days_iterator <- days_iterator + 1
        }
        data_iterator <- data_iterator + 1
    }

    data_dark_all <- data_dark_list %>% reduce(left_join, by="ElapsedTimeD")
    data_light_all <- data_light_list %>% reduce(left_join, by="ElapsedTimeD")

    circadian_list <- list(data_dark_all, data_light_all)
    names(circadian_list)[1:2] <- c("Dark Averages", "Light Averages")
    return (circadian_list)
}
