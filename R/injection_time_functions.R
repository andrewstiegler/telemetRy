#' Add a column for time since injection / break in dataset.
#'
#' This function calculates the time since injection / break in dataset. Use
#' this function on a dataframe generated from a DSI export using the
#' DSI_export_to_dataframe function.
#' @describeIn add_injtime Add injection time column to dataset.
#' @param data A dataframe created using the DSI_export_to_dataframe function.
#' @param injection_time Time before but close to the injection (24H)
#' @return A dataframe containing a column for time since injection / break and
#' columns for each parameter, which each subject synchronized to their
#' injection time.
#' @examples
#' add_injtime(data = data, lights_on = 21)
#' isolate_postinj(data = add_injtime_output, parameter = "SBP")

# faster implementation of adding injection time
add_injtime <- function (data, injection_time) {
  injection_time<-as.ITime(3600*injection_time)

  TBegin<-data$TimesOnly[1]
  TNext<-data$TimesOnly[2]
  Tdelta<-TNext-TBegin
  TotalTimes<-86400*as.numeric(max(data$Time)-min(data$Time))
  SingleSNDataList<-list()
  SingleSNDataLengthList<-list()
  injection_times<-list()

  TypicalIterator<-1

  while(TypicalIterator <= unique(data$.id)%>%length()){
    SingleSNData<-filter(data, .id == unique(data$.id)[TypicalIterator])
    SingleSNData_postinj <- filter(SingleSNData, TimesOnly > injection_time)
    FirstNA <- as.numeric(which(is.na(SingleSNData_postinj$SBP)))[1]
    InjectionCheck<-as.ITime(SingleSNData_postinj$Time[FirstNA])>injection_time
    if(InjectionCheck){
      injection_times[[TypicalIterator]] <-
        as.ITime(SingleSNData_postinj$Time[FirstNA])
    } else {
      injection_times[[TypicalIterator]]<-injection_time
    }
    inj_time_row <- which(as.ITime(SingleSNData$Time)==
                            injection_times[[TypicalIterator]])[1]
    inj_time_offset <- SingleSNData$ElapsedTime[inj_time_row]
    BuildDay<-tibble(Time=SingleSNData$Time,
                     InjTime=SingleSNData$ElapsedTime - inj_time_offset,
                     SBP=SingleSNData$SBP,
                     DBP=SingleSNData$DBP,
                     MAP=SingleSNData$MAP,
                     HR=SingleSNData$HR,
                     Temp=SingleSNData$Temp,
                     Activity=SingleSNData$Activity)

    colnames(BuildDay)[3:8]<-
      c(paste(unique(data$.id)[TypicalIterator],"SBP",sep=""),
        paste(unique(data$.id)[TypicalIterator],"DBP",sep=""),
        paste(unique(data$.id)[TypicalIterator],"MAP",sep=""),
        paste(unique(data$.id)[TypicalIterator],"HR",sep=""),
        paste(unique(data$.id)[TypicalIterator],"Temp",sep=""),
        paste(unique(data$.id)[TypicalIterator],"Activity",sep=""))

    SingleSNDataList[[TypicalIterator]] <- BuildDay
    SingleSNDataLengthList[[TypicalIterator]] <- length(SingleSNData$Time)
    TypicalIterator<-TypicalIterator+1
  }

  JoinedData <- SingleSNDataList %>% reduce(left_join, by="InjTime")
  colnames(JoinedData)[1:2] <- c("Real", "Inj")
  JoinedData <- select(JoinedData, -grep("Time", colnames(JoinedData)))
  colnames(JoinedData)[1:2] <- c("Time", "InjTime")

  return (JoinedData) }


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
