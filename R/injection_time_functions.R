#' Add a column for time since injection / break in dataset.
#'
#' This function calculates the time since injection / break in dataset. Use
#' this function on a dataframe generated from a DSI export using the
#' DSI_export_to_dataframe function.
#' @describeIn add_injtime Add injection time column to dataset.
#' @param data A dataframe created using the DSI_export_to_dataframe function.
#' @param injection_time Time before but close to the injection (24H)
#' @param parameter Name of a parameter in the dataset, in quotes.
#' @return A dataframe containing a column for time since injection / break and
#' columns for each parameter, which each subject synchronized to their
#' injection time.

# faster implementation of adding injection time
add_injtime <- function (data, injection_time) {
  injection_time_missing <- missing(injection_time)
  if (injection_time_missing) {
    stop("Injection time missing")
  }

  if (!is.numeric(injection_time)) {
    stop("Injection time must be numeric 0 >= injection_time <= 24")
  }

  if (injection_time < 0 | injection_time > 24) {
    stop("Injection time must be numeric 0 >= injection_time <= 24")
  }

  df_check <- is.data.frame(data)
  if (!df_check) stop("'data' must be dataframe.")

  injection_time<-as.ITime(3600*injection_time)

  if ("TimesOnly" %in% colnames(data)) {
  t_begin <- (data %>% select(.data$TimesOnly))[1,1]
  t_next <- (data %>% select(.data$TimesOnly))[2,1]
  } else stop("No TimesOnly column - wrong dataframe as input")

  t_delta <- t_next-t_begin

  if (max(data$ElapsedTime < 86400)) {time_multiplier <- 3600
  } else {time_multiplier <- 86400}

  total_times<-time_multiplier*as.numeric(max(data$Time)-min(data$Time))
  single_SN_data_list<-list()
  single_SN_data_length_list<-list()
  injection_times<-list()

  for (typical_iterator in 1:(unique(data$.id)%>%length())){
    single_SN_data<-filter(data, .data$.id ==
                             unique(data$.id)[typical_iterator])
    single_SN_data_postinj <- filter(single_SN_data, .data$TimesOnly >
                                       injection_time)
    first_NA <- as.numeric(which(is.na(single_SN_data_postinj$SBP)))[1]
    if (is.na(first_NA)){
      stop("No break in the data to base injection time")
    }
    injection_check <- as.ITime(single_SN_data_postinj$Time[first_NA]) >
      injection_time
    if (injection_check){
      injection_times[[typical_iterator]] <-
        as.ITime(single_SN_data_postinj$Time[first_NA])
    } else {
      injection_times[[typical_iterator]] <- injection_time
    }

    inj_time_row <- which(as.ITime(single_SN_data$Time)==
                            injection_times[[typical_iterator]])[1]
    inj_time_offset <- single_SN_data$ElapsedTime[inj_time_row]
    injection_data <- tibble(Time=single_SN_data$Time,
                     InjTime=single_SN_data$ElapsedTime - inj_time_offset,
                     SBP=single_SN_data$SBP,
                     DBP=single_SN_data$DBP,
                     MAP=single_SN_data$MAP,
                     HR=single_SN_data$HR,
                     Temp=single_SN_data$Temp,
                     Activity=single_SN_data$Activity)

    colnames(injection_data)[3:8]<-
      c(paste(unique(data$.id)[typical_iterator],"SBP",sep=""),
        paste(unique(data$.id)[typical_iterator],"DBP",sep=""),
        paste(unique(data$.id)[typical_iterator],"MAP",sep=""),
        paste(unique(data$.id)[typical_iterator],"HR",sep=""),
        paste(unique(data$.id)[typical_iterator],"Temp",sep=""),
        paste(unique(data$.id)[typical_iterator],"Activity",sep=""))

    single_SN_data_list[[typical_iterator]] <- injection_data
    single_SN_data_length_list[[typical_iterator]] <- length(single_SN_data$Time)
    typical_iterator<-typical_iterator+1
  }

  joined_data <- single_SN_data_list %>% reduce(left_join, by="InjTime")
  colnames(joined_data)[1:2] <- c("Real", "Inj")
  joined_data <- select(joined_data, -grep("Time", colnames(joined_data)))
  colnames(joined_data)[1:2] <- c("Time", "InjTime")

  return (joined_data) }


# post_inj_sbp function takes dataframe with inj_time as input
# returns only SBP columns and time columns
# same for all of DBP, MAP, HR, Temp, Activity

#' @describeIn add_injtime Isolate parameter from dataset after adding injection
#' time.
isolate_postinj <- function(data, parameter) {
  param_missing <- missing(parameter)
  if (param_missing) return ("No parameter - please set parameter to isolate")
  if (sum(grep(paste(parameter),colnames(data))) == 0) {
    return ("Parameter not found in dataset")
  }
  if (colnames(data)[1] != "Time") {
    return ("First column is not Time column - wrong data?")
  }
  postinj_param <- select(data, 1, grep(paste(parameter),
                                        colnames(data)))
  return (postinj_param)
}
