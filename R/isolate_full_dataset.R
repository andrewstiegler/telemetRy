#' Isolate parameters from complete telemetry dataframe.
#'
#' @description
#' These functions take the output of DSI_export_to_dataframe and outputs a new
#' dataframe for the specified parameter.
#' @describeIn isolate_sbp
#' @param data A dataframe which is the output from DSI_export_to_dataframe.
#' @return A dataframe with the Time and elapsed_time columns from the input,
#' and a column for each subject appended with the parameter (i.e. SBP).
#' Additionally a column for the mean of all subjects in the dataset.
#' @examples
#' isolate_sbp(exported_data)
#' isolate_dbp(exported_data)
#' isolate_map(exported_data)
#' isolate_temp(exported_data)
#' isolate_HR(exported_data)
#' isolate_activity(exported_data)
#' @describeIn isolate_sbp Isolate SBP from complete dataframe.
# isolate_sbp function takes DSI_export_to_dataframe output and isolates SBP
# keeps time columns, each SN gets an SBP column named "SNSBP"
isolate_sbp <- function (data) {
    ids <- data$.id %>% unique()
    sbp_1 <- filter(data, .id==ids[[1]]) %>% select(Time, SBP)
    colnames(sbp_1)[2] <- paste(ids[[1]], "SBP", sep="")
    sbp_joined <- list()

    for (sbp_iterator in 1:length(ids)){
        sbp_next <- filter(data, .id==ids[[sbp_iterator]]) %>%
            select(Time, SBP)
        colnames(sbp_next)[2] <- paste(ids[[sbp_iterator]],"SBP",sep="")
        sbp_joined[[sbp_iterator]] <- sbp_next
        sbp_iterator <- sbp_iterator + 1
    }

    sbp_joined_full <- sbp_joined %>% reduce(left_join,by="Time")
    sbp_joined_full$mean <- rowMeans(sbp_joined_full %>% select(-1),
                                     na.rm= TRUE)

    sbp_joined_full$elapsed_time[1] <- 0

    for (elapsed_iterator in 2:length(sbp_joined_full$Time)){
        sbp_joined_full$elapsed_time[elapsed_iterator] =
            as.numeric(sbp_joined_full$Time[elapsed_iterator]) -
            as.numeric(sbp_joined_full$Time[1])
        elapsed_iterator<-elapsed_iterator+1
    }

    return (sbp_joined_full)
}

#' @describeIn isolate_sbp Isolate DBP from complete dataframe.

# isolate_dbp function takes DSI_export_to_dataframe output and isolates DBP
# keeps time columns, each SN gets an SBP column named "SNDBP"
isolate_dbp <- function (data) {
    ids <- data$.id %>% unique()
    dbp_1 <- filter(data, .id==ids[[1]]) %>% select(Time, DBP)
    colnames(dbp_1)[2] <- paste(ids[[1]], "DBP", sep="")
    dbp_joined <- list()

    for (dbp_iterator in 1:length(ids)){
        dbp_next <- filter(data, .id==ids[[dbp_iterator]]) %>%
            select(Time, DBP)
        colnames(dbp_next)[2] <- paste(ids[[dbp_iterator]],"DBP",sep="")
        dbp_joined[[dbp_iterator]] <- dbp_next
        dbp_iterator <- dbp_iterator + 1
    }

    dbp_joined_full <- dbp_joined %>% reduce(left_join,by="Time")
    dbp_joined_full$mean <- rowMeans(dbp_joined_full %>% select(-1),
                                     na.rm= TRUE)

    dbp_joined_full$elapsed_time[1] <- 0

    for (elapsed_iterator in 2:length(dbp_joined_full$Time)){
        dbp_joined_full$elapsed_time[elapsed_iterator] =
            as.numeric(dbp_joined_full$Time[elapsed_iterator]) -
            as.numeric(dbp_joined_full$Time[1])
        elapsed_iterator<-elapsed_iterator+1
    }

    return (dbp_joined_full)
}

#' @describeIn isolate_sbp Isolate MAP from complete dataframe.

# isolate_map function takes DSI_export_to_dataframe output and isolates MAP
# keeps time columns, each SN gets an SBP column named "SNMAP"
isolate_map <- function (data) {
    ids <- data$.id %>% unique()
    map_1 <- filter(data, .id==ids[[1]]) %>% select(Time, MAP)
    colnames(map_1)[2] <- paste(ids[[1]], "MAP", sep="")
    map_joined <- list()

    for (map_iterator in 1:length(ids)){
        map_next <- filter(data, .id==ids[[map_iterator]]) %>%
            select(Time, MAP)
        colnames(map_next)[2] <- paste(ids[[map_iterator]],"MAP",sep="")
        map_joined[[map_iterator]] <- map_next
        map_iterator <- map_iterator + 1
    }

    map_joined_full <- map_joined %>% reduce(left_join,by="Time")
    map_joined_full$mean <- rowMeans(map_joined_full %>% select(-1),
                                     na.rm= TRUE)

    map_joined_full$elapsed_time[1] <- 0

    for (elapsed_iterator in 2:length(map_joined_full$Time)){
        map_joined_full$elapsed_time[elapsed_iterator] =
            as.numeric(map_joined_full$Time[elapsed_iterator]) -
            as.numeric(map_joined_full$Time[1])
        elapsed_iterator<-elapsed_iterator+1
    }

    return (map_joined_full)
}
#' @describeIn isolate_sbp Isolate temperature from complete dataframe.

# isolate_temp function takes DSI_export_to_dataframe output and isolates temp
# keeps time columns, each SN gets an SBP column named "SNTemp"
isolate_temp <- function (data) {
    ids <- data$.id %>% unique()
    temp_1 <- filter(data, .id==ids[[1]]) %>% select(Time, Temp)
    colnames(temp_1)[2] <- paste(ids[[1]], "Temp", sep="")
    temp_joined <- list()

    for (temp_iterator in 1:length(ids)){
        temp_next <- filter(data, .id==ids[[temp_iterator]]) %>%
            select(Time, Temp)
        colnames(temp_next)[2] <- paste(ids[[temp_iterator]],"Temp",sep="")
        temp_joined[[temp_iterator]] <- temp_next
        temp_iterator <- temp_iterator + 1
    }

    temp_joined_full <- temp_joined %>% reduce(left_join,by="Time")
    temp_joined_full$mean <- rowMeans(temp_joined_full %>% select(-1),
                                     na.rm= TRUE)

    temp_joined_full$elapsed_time[1] <- 0

    for (elapsed_iterator in 2:length(temp_joined_full$Time)){
        temp_joined_full$elapsed_time[elapsed_iterator] =
            as.numeric(temp_joined_full$Time[elapsed_iterator]) -
            as.numeric(temp_joined_full$Time[1])
        elapsed_iterator<-elapsed_iterator+1
    }

    return (temp_joined_full)
}
#' @describeIn isolate_sbp Isolate HR from complete dataframe.

# isolate_HR function takes DSI_export_to_dataframe output and isolates HR
# keeps time columns, each SN gets an SBP column named "SNHR"
isolate_HR <- function (data) {
    ids <- data$.id %>% unique()
    HR_1 <- filter(data, .id==ids[[1]]) %>% select(Time, HR)
    colnames(HR_1)[2] <- paste(ids[[1]], "HR", sep="")
    HR_joined <- list()

    for (HR_iterator in 1:length(ids)){
        HR_next <- filter(data, .id==ids[[HR_iterator]]) %>%
            select(Time, HR)
        colnames(HR_next)[2] <- paste(ids[[HR_iterator]],"HR",sep="")
        HR_joined[[HR_iterator]] <- HR_next
        HR_iterator <- HR_iterator + 1
    }

    HR_joined_full <- HR_joined %>% reduce(left_join,by="Time")
    HR_joined_full$mean <- rowMeans(HR_joined_full %>% select(-1),
                                     na.rm= TRUE)

    HR_joined_full$elapsed_time[1] <- 0

    for (elapsed_iterator in 2:length(HR_joined_full$Time)){
        HR_joined_full$elapsed_time[elapsed_iterator] =
            as.numeric(HR_joined_full$Time[elapsed_iterator]) -
            as.numeric(HR_joined_full$Time[1])
        elapsed_iterator<-elapsed_iterator+1
    }

    return (HR_joined_full)
}
#' @describeIn isolate_sbp Isolate activity from complete dataframe.

# isolate_activity function takes DSI_export_to_dataframe output and isolates activity
# keeps time columns, each SN gets an SBP column named "SNActivity"
isolate_activity <- function (data) {
    ids <- data$.id %>% unique()
    activity_1 <- filter(data, .id==ids[[1]]) %>% select(Time, Activity)
    colnames(activity_1)[2] <- paste(ids[[1]], "Activity", sep="")
    activity_joined <- list()

    for (activity_iterator in 1:length(ids)){
        activity_next <- filter(data, .id==ids[[activity_iterator]]) %>%
            select(Time, Activity)
        colnames(activity_next)[2] <- paste(ids[[activity_iterator]],"Activity",sep="")
        activity_joined[[activity_iterator]] <- activity_next
        activity_iterator <- activity_iterator + 1
    }

    activity_joined_full <- activity_joined %>% reduce(left_join,by="Time")
    activity_joined_full$mean <- rowMeans(activity_joined_full %>% select(-1),
                                     na.rm= TRUE)

    activity_joined_full$elapsed_time[1] <- 0

    for (elapsed_iterator in 2:length(activity_joined_full$Time)){
        activity_joined_full$elapsed_time[elapsed_iterator] =
            as.numeric(activity_joined_full$Time[elapsed_iterator]) -
            as.numeric(activity_joined_full$Time[1])
        elapsed_iterator<-elapsed_iterator+1
    }

    return (activity_joined_full)
}
