# generate_SNList function takes filepath of DSI exported Excel file as input
# Returns list of telemeter SNs as vector
generate_SNList <- function(SelectedFile) {
SheetList <- excel_sheets(SelectedFile)
sheetloopiterator <- 1
SNList <- 1
SNfirstloop <- 1
DataList <- list()
while (sheetloopiterator <= length(SheetList)){
  if(substr(SheetList[sheetloopiterator],0,1) == "P"){
    SNList[SNfirstloop] <- substr(SheetList[sheetloopiterator], 20, 26)
    #Check if the SN is 6 or 7 digits
    if(substr(SNList[SNfirstloop], 7, 7) == ")")
      SNList[SNfirstloop] <- substr(SheetList[sheetloopiterator], 20, 25)
    SNfirstloop <- SNfirstloop + 1
  }
  sheetloopiterator<- sheetloopiterator + 1}
return (SNList)
}

# DSI_export_to_dataframe function takes same input filepath of DSI exported 
# Excel file as input
# Returns a dataframe with columns: 
# .id (telemeter SN), Time, ElapsedTime (various units), BP parameters
DSI_export_to_dataframe <- function (selected_file) {
  sheet_list <- excel_sheets(selected_file)
  sheet_loop_iterator <- 1
  SN_list <- 1
  SN_first_loop <- 1
  import_data_list <- list()
  while (sheet_loop_iterator <= length(sheet_list)){
    if (substr(sheet_list[sheet_loop_iterator],0,1) == "P") {
      SN_list[SN_first_loop] <- 
        substr(sheet_list[sheet_loop_iterator], 20, 26)
      #Check if the SN is 6 or 7 digits
      if(substr(SN_list[SN_first_loop], 7, 7) == ")")
        SN_list[SN_first_loop] <- 
          substr(sheet_list[sheet_loop_iterator], 20, 25)
      SN_first_loop <- SN_first_loop + 1
    }
    sheet_loop_iterator<- sheet_loop_iterator + 1}
  SN_iterator <- 1
  while (SN_iterator <= length(SN_list)) {
    progress(SN_iterator*12)
    #Import the first sheet of the file by matching SN
    imported_sheet <- read_excel(
      selected_file, 
      sheet = 
        paste("Parameters_HD-X10 (",SN_list[SN_iterator],")"
              ,sep=""),
      col_names = as.character(c(1:18))) %>%
      # Select only the columns that we care about for analysis,
      # excluding duplicates and battery life etc
      select(-2, -3, -4, -5, -6, -7, -8, -14, -15, -17, -18) %>%
      rename(Time = `1`, SBP = `9`, DBP = `10`, MAP = `11`, HR = `12`, 
             Temp = `13`, Activity = `16`) %>%
      tail(-3)
    #Fix time from excel import
    imported_sheet$Time<-excel_numeric_to_date(as.numeric(imported_sheet$Time),
                                               include_time = TRUE)
    
    #DSI S/N's can be 6 or 7 numbers long, we want SN to reflect this
    SN <- substr(sheet_list[SN_iterator], 20, 26)
    if(substr(SN, 7, 7) == ")")
      SN <- substr(sheet_list[SN_iterator], 20, 25)
    
    #Create new columns for elapsed time  
    elapsed_iterator <- 1
    imported_sheet$ElapsedTime <- NA
    imported_sheet$ElapsedTime[elapsed_iterator] <- 0
    
    #Loop over entire recording to calculate elapsed time for each row
    elapsed_iterator <- 2
    while (elapsed_iterator <= length(imported_sheet$Time)){
      imported_sheet$ElapsedTime[elapsed_iterator] <- 
        as.numeric(imported_sheet$Time[elapsed_iterator]) - 
        as.numeric(imported_sheet$Time[1])
      elapsed_iterator <- elapsed_iterator+1
    }
    #Convert elapsed time into other units for ease of plotting later
    imported_sheet$ElapsedTimeMin <- imported_sheet$ElapsedTime/60
    imported_sheet$ElapsedTimeH <- imported_sheet$ElapsedTime/3600
    imported_sheet$ElapsedTimeD <- imported_sheet$ElapsedTime/86400
    #Convert elapsed time into only times for calculating lights on/off
    imported_sheet$TimesOnly <- as.ITime(imported_sheet$Time)
    #Rearrange columns for ease of reading
    imported_sheet <- select(imported_sheet,Time,TimesOnly,ElapsedTime,
                             ElapsedTimeMin,ElapsedTimeH,ElapsedTimeD,
                             SBP,DBP,MAP,HR,Temp,Activity)
    #Fix character vectors back into numeric
    imported_sheet$SBP <- as.numeric(imported_sheet$SBP)
    imported_sheet$DBP <- as.numeric(imported_sheet$DBP)
    imported_sheet$MAP <- as.numeric(imported_sheet$MAP)
    imported_sheet$HR <- as.numeric(imported_sheet$HR)
    imported_sheet$Temp <- as.numeric(imported_sheet$Temp)
    imported_sheet$Activity <- as.numeric(imported_sheet$Activity)
    #Fix mean for physician's mean BP
    imported_sheet$MAP <- imported_sheet$DBP + 
      (imported_sheet$SBP-imported_sheet$DBP)/3
    
    #Create a list of all data frames imported from excel file  
    import_data_list[[SN_list[SN_iterator]]] <- imported_sheet
    SN_iterator <- SN_iterator + 1
  }
  
  #Join all data into one table with a label column for SN
  all_import_data <- rbindlist(import_data_list, idcol = TRUE)
  
  return (all_import_data) 
}

# isolate_sbp function takes DSI_export_to_dataframe output and isolates SBP
# keeps time columns, each SN gets an SBP column named "SNSBP"
isolate_sbp <- function (sbp_data) {
  ids <- sbp_data$.id %>% unique()
  sbp_1 <- filter(sbp_data, .id==ids[[1]]) %>% select(Time, SBP)
  colnames(sbp_1)[2] <- paste(ids[[1]],"SBP",sep="")
  sbp_joined <- list()
  sys_looper <- 1
  
  while (sys_looper <= length(ids)){
    sbp_next <- filter(sbp_data, .id==ids[[sys_looper]]) %>% select(Time, SBP)
    colnames(sbp_next)[2] <- paste(ids[[sys_looper]],"SBP",sep="")
    sbp_joined[[sys_looper]] <- sbp_next
    sys_looper <- sys_looper + 1
  }
  
  sbp_joined_full <- sbp_joined%>%reduce(left_join,by="Time")
  sbp_joined_full$mean <- rowMeans(sbp_joined_full %>% select(-1), na.rm= TRUE)

  ElapsedIterator <- 1
  ElapsedTime <- 0
  sbp_joined_full$ElapsedTime[ElapsedIterator]<-ElapsedTime
  ElapsedIterator<-2
  
  while(ElapsedIterator<=length(sbp_joined_full$Time)){
    sbp_joined_full$ElapsedTime[ElapsedIterator] = as.numeric(sbp_joined_full$Time[ElapsedIterator])-as.numeric(sbp_joined_full$Time[1])
    ElapsedIterator<-ElapsedIterator+1
  }
  
  return (sbp_joined_full)}

# isolate_dbp function takes DSI_export_to_dataframe output and isolates DBP
# keeps time columns, each SN gets an SBP column named "SNDBP"
isolate_dbp <- function (dbp_data) {
  ids <- dbp_data$.id %>% unique()
  dbp_1 <- filter(dbp_data, .id==ids[[1]]) %>% select(Time, DBP)
  colnames(dbp_1)[2] <- paste(ids[[1]],"DBP",sep="")
  dbp_joined <- list()
  dbp_looper <- 1
  
  while (dbp_looper <= length(ids)){
    dbp_next <- filter(dbp_data, .id==ids[[dbp_looper]]) %>% select(Time, DBP)
    colnames(dbp_next)[2] <- paste(ids[[dbp_looper]],"DBP",sep="")
    dbp_joined[[dbp_looper]] <- dbp_next
    dbp_looper <- dbp_looper + 1
  }
  
  dbp_joined_full <- dbp_joined%>%reduce(left_join,by="Time")
  dbp_joined_full$mean <- rowMeans(dbp_joined_full %>% select(-1), na.rm= TRUE)
  
  ElapsedIterator <- 1
  ElapsedTime <- 0
  dbp_joined_full$ElapsedTime[ElapsedIterator]<-ElapsedTime
  ElapsedIterator<-2
  
  while(ElapsedIterator<=length(dbp_joined_full$Time)){
    dbp_joined_full$ElapsedTime[ElapsedIterator] = as.numeric(dbp_joined_full$Time[ElapsedIterator])-as.numeric(dbp_joined_full$Time[1])
    ElapsedIterator<-ElapsedIterator+1
  }
  
  return (dbp_joined_full)}

# isolate_map function takes DSI_export_to_dataframe output and isolates MAP
# keeps time columns, each SN gets an SBP column named "SNMAP"
isolate_map <- function (map_data) {
  ids <- map_data$.id %>% unique()
  map_1 <- filter(map_data, .id==ids[[1]]) %>% select(Time, MAP)
  colnames(map_1)[2] <- paste(ids[[1]],"MAP",sep="")
  map_joined <- list()
  map_looper <- 1
  
  while (map_looper <= length(ids)){
    map_next <- filter(map_data, .id==ids[[map_looper]]) %>% select(Time, MAP)
    colnames(map_next)[2] <- paste(ids[[map_looper]],"MAP",sep="")
    map_joined[[map_looper]] <- map_next
    map_looper <- map_looper + 1
  }
  
  map_joined_full <- map_joined%>%reduce(left_join,by="Time")
  map_joined_full$mean <- rowMeans(map_joined_full %>% select(-1), na.rm= TRUE)
  
  return (map_joined_full)}

# isolate_temp function takes DSI_export_to_dataframe output and isolates temp
# keeps time columns, each SN gets an SBP column named "SNTemp"
isolate_temp <- function (temp_data) {
  ids <- temp_data$.id %>% unique()
  temp_1 <- filter(temp_data, .id==ids[[1]]) %>% select(Time, Temp)
  colnames(temp_1)[2] <- paste(ids[[1]],"Temp",sep="")
  temp_joined <- list()
  temp_looper <- 1
  
  while (temp_looper <= length(ids)){
    temp_next <- filter(temp_data, .id==ids[[temp_looper]]) %>% select(Time, Temp)
    colnames(temp_next)[2] <- paste(ids[[temp_looper]],"Temp",sep="")
    temp_joined[[temp_looper]] <- temp_next
    temp_looper <- temp_looper + 1
  }
  
  temp_joined_full <- temp_joined%>%reduce(left_join,by="Time")
  temp_joined_full$mean <- rowMeans(temp_joined_full %>% select(-1), na.rm= TRUE)
  
  ElapsedIterator <- 1
  ElapsedTime <- 0
  temp_joined_full$ElapsedTime[ElapsedIterator]<-ElapsedTime
  ElapsedIterator<-2
  
  while(ElapsedIterator<=length(temp_joined_full$Time)){
    temp_joined_full$ElapsedTime[ElapsedIterator] = as.numeric(temp_joined_full$Time[ElapsedIterator])-as.numeric(temp_joined_full$Time[1])
    ElapsedIterator<-ElapsedIterator+1
  }
  
  return (temp_joined_full)}

# isolate_HR function takes DSI_export_to_dataframe output and isolates HR
# keeps time columns, each SN gets an SBP column named "SNHR"
isolate_HR <- function (HR_data) {
  ids <- HR_data$.id %>% unique()
  HR_1 <- filter(HR_data, .id==ids[[1]]) %>% select(Time, HR)
  colnames(HR_1)[2] <- paste(ids[[1]],"HR",sep="")
  HR_joined <- list()
  HR_looper <- 1
  
  while (HR_looper <= length(ids)){
    HR_next <- filter(HR_data, .id==ids[[HR_looper]]) %>% select(Time, HR)
    colnames(HR_next)[2] <- paste(ids[[HR_looper]],"HR",sep="")
    HR_joined[[HR_looper]] <- HR_next
    HR_looper <- HR_looper + 1
  }
  
  HR_joined_full <- HR_joined%>%reduce(left_join,by="Time")
  HR_joined_full$mean <- rowMeans(HR_joined_full %>% select(-1), na.rm= TRUE)
  
  ElapsedIterator <- 1
  ElapsedTime <- 0
  HR_joined_full$ElapsedTime[ElapsedIterator]<-ElapsedTime
  ElapsedIterator<-2
  
  while(ElapsedIterator<=length(HR_joined_full$Time)){
    HR_joined_full$ElapsedTime[ElapsedIterator] = as.numeric(HR_joined_full$Time[ElapsedIterator])-as.numeric(HR_joined_full$Time[1])
    ElapsedIterator<-ElapsedIterator+1
  }
  
  return (HR_joined_full)}

# isolate_activity function takes DSI_export_to_dataframe output and isolates activity
# keeps time columns, each SN gets an SBP column named "SNActivity"
isolate_activity <- function (activity_data) {
  ids <- activity_data$.id %>% unique()
  activity_1 <- filter(activity_data, .id==ids[[1]]) %>% select(Time, Activity)
  colnames(activity_1)[2] <- paste(ids[[1]],"activity",sep="")
  activity_joined <- list()
  activity_looper <- 1
  
  while (activity_looper <= length(ids)){
    activity_next <- filter(activity_data, .id==ids[[activity_looper]]) %>% select(Time, Activity)
    colnames(activity_next)[2] <- paste(ids[[activity_looper]],"activity",sep="")
    activity_joined[[activity_looper]] <- activity_next
    activity_looper <- activity_looper + 1
  }
  
  activity_joined_full <- activity_joined%>%reduce(left_join,by="Time")
  activity_joined_full$mean <- rowMeans(activity_joined_full %>% select(-1), na.rm= TRUE)
  
  ElapsedIterator <- 1
  ElapsedTime <- 0
  activity_joined_full$ElapsedTime[ElapsedIterator]<-ElapsedTime
  ElapsedIterator<-2
  
  while(ElapsedIterator<=length(activity_joined_full$Time)){
    activity_joined_full$ElapsedTime[ElapsedIterator] = as.numeric(activity_joined_full$Time[ElapsedIterator])-as.numeric(activity_joined_full$Time[1])
    ElapsedIterator<-ElapsedIterator+1
  }
  
  return (activity_joined_full)}

# hourly_average function takes dataframe of isolated parameters as input
# returns hourly average of parameter and keeps time columns
hourly_average <- function(avg_data_tibble) {
  
  rows_avg_iterator <- 1
  seconds_per_row <- avg_data_tibble$ElapsedTime[2] - avg_data_tibble$ElapsedTime[1]
  rows_per_hour <- 3600 / seconds_per_row
  avg_data_tibble_notime <- avg_data_tibble %>% select(-1)
  while (rows_avg_iterator<=nrow(avg_data_tibble)/rows_per_hour) {
    avg_data_tibble_notime[rows_avg_iterator,] <- avg_data_tibble_notime %>% 
      slice((rows_avg_iterator*rows_per_hour-(rows_per_hour-1)):(rows_avg_iterator*rows_per_hour)) %>%
      colMeans(na.rm=TRUE) %>%
      stack() %>%
      as_tibble() %>%
      spread(key=ind, value=values)
    rows_avg_iterator <- rows_avg_iterator+1
  }
  
  avg_data_tibble_notime <- head(avg_data_tibble_notime,nrow(avg_data_tibble_notime)/rows_per_hour)
  avg_data_tibble_notime$Time <- avg_data_tibble$Time[1] + avg_data_tibble_notime$ElapsedTime
  avg_data_tibble_notime <- avg_data_tibble_notime %>% select(Time, ElapsedTime, everything())
  return(avg_data_tibble_notime) }

# add_inj_time function takes dataframe as input, requires gap of NA values
# NA values serve as time for injections, groups together and outputs
# single dataframe with real time, injection time, and BP parameters
# for each SN
add_inj_time <- function (AllData, InjectionTime) {
  
  # InjectionTime<-as.numeric(readline
  #                           (prompt = "Approx first injection time? (24H) "))
  InjectionTime<-as.ITime(3600*InjectionTime)
  
  TBegin<-AllData$TimesOnly[1]
  TNext<-AllData$TimesOnly[2]
  Tdelta<-TNext-TBegin
  TotalTimes<-86400*as.numeric(max(AllData$Time)-min(AllData$Time))
  SingleSNDataList<-list()
  SingleSNDataLengthList<-list()
  InjectionTimes<-list()
  
  #Set iterators to 1 to loop over all times and serial numbers for export
  TypicalIterator<-1
  TimesIterator<-1
  while(TypicalIterator <= unique(AllData$.id)%>%length()){
    SingleSNData<-filter(AllData, .id == unique(AllData$.id)[TypicalIterator])
    SingleSNData_postinj <- filter(SingleSNData, TimesOnly > InjectionTime)
    FirstNA<-as.numeric(which(is.na(SingleSNData_postinj$SBP))[1])
    InjectionCheck<-as.ITime(SingleSNData_postinj$Time[FirstNA])>InjectionTime
    if(InjectionCheck){
      InjectionTimes[[TypicalIterator]]<-as.ITime(SingleSNData_postinj$Time[FirstNA])
    } else {
      InjectionTimes[[TypicalIterator]]<-InjectionTime
    }
    BuildDay<-tibble(Time=seq.POSIXt(from=min(AllData$Time),to=max(AllData$Time), by=Tdelta),
                     InjTime=seq(from=1,to=TotalTimes/as.numeric(Tdelta)+1),
                     SBP=seq(from=1,to=TotalTimes/as.numeric(Tdelta)+1),
                     DBP=seq(from=1,to=TotalTimes/as.numeric(Tdelta)+1),
                     MAP=seq(from=1,to=TotalTimes/as.numeric(Tdelta)+1),
                     HR=seq(from=1,to=TotalTimes/as.numeric(Tdelta)+1),
                     Temp=seq(from=1,to=TotalTimes/as.numeric(Tdelta)+1),
                     Activity=seq(from=1,to=TotalTimes/as.numeric(Tdelta)+1)) %>% 
      head(length(SingleSNData$Time))
    
    while(TimesIterator<=length(SingleSNData$Time)){
      SingleSNSingleTime<-SingleSNData[TimesIterator,]
      BuildDay$Time[TimesIterator]<-as.POSIXct(SingleSNSingleTime[1,2])
      BuildDay$InjTime[TimesIterator]<-(TimesIterator-1)*10+as.numeric((as.ITime(SingleSNData$Time[1])-InjectionTimes[[TypicalIterator]]))
      BuildDay$SBP[TimesIterator]<-SingleSNSingleTime[1,8]
      BuildDay$DBP[TimesIterator]<-SingleSNSingleTime[1,9]
      BuildDay$MAP[TimesIterator]<-SingleSNSingleTime[1,10]
      BuildDay$HR[TimesIterator]<-SingleSNSingleTime[1,11]
      BuildDay$Temp[TimesIterator]<-SingleSNSingleTime[1,12]
      BuildDay$Activity[TimesIterator]<-SingleSNSingleTime[1,13]
      if(TimesIterator==length(SingleSNData$Time)){
        #colnames(BuildDay)[2]<-paste(unique(AllData$.id)[TypicalIterator],"InjTime",sep="")
        colnames(BuildDay)[3:8]<-
          c(paste(unique(AllData$.id)[TypicalIterator],"SBP",sep=""),
            paste(unique(AllData$.id)[TypicalIterator],"DBP",sep=""),
            paste(unique(AllData$.id)[TypicalIterator],"MAP",sep=""),
            paste(unique(AllData$.id)[TypicalIterator],"HR",sep=""),
            paste(unique(AllData$.id)[TypicalIterator],"Temp",sep=""),
            paste(unique(AllData$.id)[TypicalIterator],"Activity",sep=""))
        BuildDay %>% head(length(SingleSNData$Time))
        SingleSNDataList[[TypicalIterator]] <- BuildDay
        SingleSNDataLengthList[[TypicalIterator]] <- length(SingleSNData$Time)
      }
      TimesIterator<-TimesIterator+1
    }
    TimesIterator<-1
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
post_inj_sbp <- function (data) {
  sbp <- select(data, 1, 2, grep("SBP", colnames(data)))
  sbp$mean <- sbp %>% select(-1,-2) %>% rowMeans(na.rm = TRUE)
  return (sbp)
}
post_inj_dbp <- function (data) {
  dbp <- select(data, 1, 2, grep("DBP", colnames(data)))
  dbp$mean <- dbp %>% select(-1,-2) %>% rowMeans(na.rm = TRUE)
  return (dbp)
}
post_inj_map <- function (data) {
  map <- select(data, 1, 2, grep("MAP", colnames(data)))
  map$mean <- map %>% select(-1,-2) %>% rowMeans(na.rm = TRUE)
  return (map)
}
post_inj_temp <- function (data) {
  temp <- select(data, 1, 2, grep("Temp", colnames(data)))
  temp$mean <- temp %>% select(-1,-2) %>% rowMeans(na.rm = TRUE)
  return (temp)
}
post_inj_HR <- function (data) {
  HR <- select(data, 1, 2, grep("HR", colnames(data)))
  HR$mean <- HR %>% select(-1,-2) %>% rowMeans(na.rm = TRUE)
  return (HR)
}
post_inj_activity <- function (data) {
  activity <- select(data, 1, 2, grep("Activity", colnames(data)))
  activity$mean <- activity %>% select(-1,-2) %>% rowMeans(na.rm = TRUE)
  return (activity)
}

# post_inj_min function takes parameter-specific dataframe as input
# i.e. output of post_inj_sbp
# caclulates minimum 5-minute average after injection (200s to 6 hrs post-inj)
post_inj_min <- function (data) {
  
  SNs <- colnames(select(data,-mean,-se,-InjTime,-InjTimeH))
  # Data sets are 10 s/row, so to average over 5 min
  rows_to_avg_num <- 30
  
  # Set how many hours post-injection to look for minima
  hours_post <- 3
  second_offset <- 200
  data_for_min_data_trim <- data %>%
    filter(InjTime > second_offset & InjTime <= 3600*hours_post) %>%
    select(-mean,-se,-InjTime,-InjTimeH)
  
  avg_data_to_use_data <- data_for_min_data_trim
  rows_avg_iterator <- 1
  avg_data_tibble <- avg_data_to_use_data
  while (rows_avg_iterator<=nrow(avg_data_to_use_data)/rows_to_avg_num) {
    avg_data_tibble[rows_avg_iterator,] <- avg_data_to_use_data %>%
      slice((rows_avg_iterator*rows_to_avg_num-(rows_to_avg_num-1)):
              (rows_avg_iterator*rows_to_avg_num)) %>%
      colMeans(na.rm=TRUE) %>%
      stack() %>%
      as_tibble() %>%                               
      spread(key=ind,value=values)
    
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
  return (min_tibble) }

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
      spread(key=ind, value=values)
    rows_avg_iterator <- rows_avg_iterator+1
  }
  
  data <- head(data,nrow(data)/rows_per_hour)
  return(data) }

# faster implementation of adding injection time
good_add_injtime <- function (AllData, InjectionTime) {
  InjectionTime<-as.ITime(3600*InjectionTime)
  
  TBegin<-AllData$TimesOnly[1]
  TNext<-AllData$TimesOnly[2]
  Tdelta<-TNext-TBegin
  TotalTimes<-86400*as.numeric(max(AllData$Time)-min(AllData$Time))
  SingleSNDataList<-list()
  SingleSNDataLengthList<-list()
  InjectionTimes<-list()
  
  TypicalIterator<-1
  
  while(TypicalIterator <= unique(AllData$.id)%>%length()){
    SingleSNData<-filter(AllData, .id == unique(AllData$.id)[TypicalIterator])
    SingleSNData_postinj <- filter(SingleSNData, TimesOnly > InjectionTime)
    FirstNA <- as.numeric(which(is.na(SingleSNData_postinj$SBP)))[1]
    InjectionCheck<-as.ITime(SingleSNData_postinj$Time[FirstNA])>InjectionTime
    if(InjectionCheck){
      InjectionTimes[[TypicalIterator]] <- 
        as.ITime(SingleSNData_postinj$Time[FirstNA])
    } else {
      InjectionTimes[[TypicalIterator]]<-InjectionTime
    }
    inj_time_row <- which(as.ITime(SingleSNData$Time)==
                            InjectionTimes[[TypicalIterator]])[1]
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
      c(paste(unique(AllData$.id)[TypicalIterator],"SBP",sep=""),
        paste(unique(AllData$.id)[TypicalIterator],"DBP",sep=""),
        paste(unique(AllData$.id)[TypicalIterator],"MAP",sep=""),
        paste(unique(AllData$.id)[TypicalIterator],"HR",sep=""),
        paste(unique(AllData$.id)[TypicalIterator],"Temp",sep=""),
        paste(unique(AllData$.id)[TypicalIterator],"Activity",sep=""))
    
    SingleSNDataList[[TypicalIterator]] <- BuildDay
    SingleSNDataLengthList[[TypicalIterator]] <- length(SingleSNData$Time)
    TypicalIterator<-TypicalIterator+1
  }
  
  JoinedData <- SingleSNDataList %>% reduce(left_join, by="InjTime")
  colnames(JoinedData)[1:2] <- c("Real", "Inj")
  JoinedData <- select(JoinedData, -grep("Time", colnames(JoinedData)))
  colnames(JoinedData)[1:2] <- c("Time", "InjTime")
  
  return (JoinedData) }

# calculate typical days from single data set
typical_day <- function (data, LightsOn) {
  
#Create 24-hour averages for baseline and hypertensive data for comparisons
TBegin<-data$TimesOnly[1]
TNext<-data$TimesOnly[2]
Tdelta<-TNext-TBegin
TotalTimes<-as.numeric(86400/Tdelta)
  
#Split up by serial and average same times
TypicalIterator<-1
TimesIterator<-1
SingleSNData<-filter(data, .id == unique(data$.id)[TypicalIterator])
BuildDay<-tibble(Time=c(1:TotalTimes), SBP=c(1:TotalTimes),
                   DBP=c(1:TotalTimes),MAP=c(1:TotalTimes),
                   HR=c(1:TotalTimes),Temp=c(1:TotalTimes),
                 Activity=c(1:TotalTimes))
  typical_list <- list()
  
  while(TimesIterator<=TotalTimes){
    SingleSNSingleTime<-filter(SingleSNData, TimesOnly == 
                                 as.ITime(SingleSNData$TimesOnly[1]+Tdelta*(TimesIterator-1)))
    SingleSNSingleTimeClean<-select(SingleSNSingleTime, -.id, -Time, -TimesOnly)
    SingleSNSingleTimeAvg<-colMeans(SingleSNSingleTimeClean, na.rm = TRUE)
    BuildDay$Time[TimesIterator]<-
      SingleSNData$TimesOnly[1]+Tdelta*(TimesIterator-1)
    BuildDay$SBP[TimesIterator]<-SingleSNSingleTimeAvg[5]
    BuildDay$DBP[TimesIterator]<-SingleSNSingleTimeAvg[6]
    BuildDay$MAP[TimesIterator]<-SingleSNSingleTimeAvg[7]
    BuildDay$HR[TimesIterator]<-SingleSNSingleTimeAvg[8]
    BuildDay$Temp[TimesIterator]<-SingleSNSingleTimeAvg[9]
    BuildDay$Activity[TimesIterator]<-SingleSNSingleTimeAvg[10]
    if(TimesIterator==TotalTimes){
      BuildDay$Time<-as.ITime(round(BuildDay$Time,1))
      colnames(BuildDay)[2]<-paste(unique(data$.id)[TypicalIterator],
                                   "SBP",sep="")
      colnames(BuildDay)[3]<-paste(unique(data$.id)[TypicalIterator],
                                   "DBP",sep="")
      colnames(BuildDay)[4]<-paste(unique(data$.id)[TypicalIterator],
                                   "MAP",sep="")
      colnames(BuildDay)[5]<-paste(unique(data$.id)[TypicalIterator],
                                   "HR",sep="")
      colnames(BuildDay)[6]<-paste(unique(data$.id)[TypicalIterator],
                                   "Temp",sep="")
      colnames(BuildDay)[7]<-paste(unique(data$.id)[TypicalIterator],
                                   "Activity",sep="")
      typical_list[[TypicalIterator]] <- BuildDay
      BuildDayTemplate<-BuildDay}
    TimesIterator<-TimesIterator+1
  }
  
  TypicalIterator<-2
  while (TypicalIterator <= length(unique(data$.id))) {
    TimesIterator<-1
    SingleSNData<-filter(data, .id == unique(data$.id)[TypicalIterator])
    BuildDay<-tibble(Time=c(1:TotalTimes),SBP=c(1:TotalTimes),DBP=c(1:TotalTimes),MAP=c(1:TotalTimes),HR=c(1:TotalTimes),Temp=c(1:TotalTimes))
    while(TimesIterator<=TotalTimes){
      SingleSNSingleTime<-filter(SingleSNData, TimesOnly == as.ITime(SingleSNData$TimesOnly[1]+Tdelta*(TimesIterator-1)))
      SingleSNSingleTimeClean<-select(SingleSNSingleTime, -.id, -Time, -TimesOnly)
      SingleSNSingleTimeAvg<-colMeans(SingleSNSingleTimeClean, na.rm = TRUE)
      BuildDay$Time[TimesIterator]<-SingleSNData$TimesOnly[1]+Tdelta*(TimesIterator-1)
      BuildDay$SBP[TimesIterator]<-SingleSNSingleTimeAvg[5]
      BuildDay$DBP[TimesIterator]<-SingleSNSingleTimeAvg[6]
      BuildDay$MAP[TimesIterator]<-SingleSNSingleTimeAvg[7]
      BuildDay$HR[TimesIterator]<-SingleSNSingleTimeAvg[8]
      BuildDay$Temp[TimesIterator]<-SingleSNSingleTimeAvg[9]
      BuildDay$Activity[TimesIterator]<-SingleSNSingleTimeAvg[10]
      if(TimesIterator==TotalTimes){
        BuildDay$Time<-as.ITime(BuildDay$Time)
        colnames(BuildDay)[2]<-paste(unique(data$.id)[TypicalIterator],"Sys",sep="")
        colnames(BuildDay)[3]<-paste(unique(data$.id)[TypicalIterator],"Dia",sep="")
        colnames(BuildDay)[4]<-paste(unique(data$.id)[TypicalIterator],"MAP",sep="")
        colnames(BuildDay)[5]<-paste(unique(data$.id)[TypicalIterator],"HR",sep="")
        colnames(BuildDay)[6]<-paste(unique(data$.id)[TypicalIterator],"Temp",sep="")
        colnames(BuildDay)[7]<-paste(unique(data$.id)[TypicalIterator],"Activity",sep="")
        typical_list[[TypicalIterator]] <- BuildDay
        BuildDayTemplate<-left_join(BuildDayTemplate,BuildDay, by = "Time")
      }
      TimesIterator<-TimesIterator+1}
    TypicalIterator<-TypicalIterator+1
  }
  
  #Group all S/Ns into one file and rename for future work
  AllTypical<-distinct(BuildDayTemplate)
  
  LightsOn <- 3600 * LightsOn
  LightsOff <- 12 * 3600 + LightsOn
  if (LightsOff > 86400) {
    LightsOff <- LightsOff - 86400
  }
  
  if (LightsOn < LightsOff) {
    Lights_zero <- "Off"
  } else (Lights_zero <- "On")
  
  if (Lights_zero == "On") {
    AllTypical <- AllTypical %>% mutate(
      Lights = case_when(
        Time >= 0 & Time < LightsOff ~ "On",
        Time >= LightsOff & Time < LightsOn ~ "Off",
        Time >= LightsOn ~ "Off"
      )
    ) 
  }
  
  if (Lights_zero == "Off") {
    AllTypical <- AllTypical %>% mutate(
      Lights = case_when(
        Time >= 0 & Time < LightsOn ~ "Off",
        Time >= LightsOn & Time < LightsOff ~ "On",
        Time >= LightsOff ~ "Off"
      )
    )
  }
  
  return (AllTypical) }

# isolate typical parameters from typical day dataset
typical_sbp <- function (data) {
  sbp_typical <- select(data, 1, grep("SBP", colnames(data)))
  return (sbp_typical)}
typical_dbp <- function (data) {
  dbp_typical <- select(data, 1, grep("Sys", colnames(data)))
  return (dbp_typical)}
typical_map <- function (data) {
  map_typical <- select(data, 1, grep("MAP", colnames(data)))
  return (map_typical)}
typical_temp <- function (data) {
  temp_typical <- select(data, 1, grep("Temp", colnames(data)))
  return (temp_typical)}
typical_HR <- function (data) {
  HR_typical <- select(data, 1, grep("HR", colnames(data)))
  return (HR_typical)}
typical_activity <- function (data) {
  activity_typical <- select(data, 1, grep("Activity", colnames(data)))
  return (typical_activity)}

# calculate average parameters in dark or light conditions ####
# circadian_avg function takes imported dataframe and LightsOn time as input
# returns list of 2 elements - dark parameters, and light parameters
circadian_avg <- function (data, LightsOn) {
  data<-J10B
  LightsOn <- 6
  data_length <- length(data$.id %>% unique())
  SNs <- data$.id %>% unique()
  
  LightsOn <- 3600 * LightsOn
  LightsOff <- 12 * 3600 + LightsOn
  if (LightsOff > 86400) {
    LightsOff <- LightsOff - 86400
  }
  
  if (LightsOn < LightsOff) {
    Lights_zero <- "Off"
  } else (Lights_zero <- "On")
  
  if (Lights_zero == "On") {
    data <- data %>% mutate(
      Lights = case_when(
        TimesOnly >= 0 & TimesOnly < LightsOff ~ "On",
        TimesOnly >= LightsOff & TimesOnly < LightsOn ~ "Off",
        TimesOnly >= LightsOn ~ "Off"
      )
    ) 
  }
  
  if (Lights_zero == "Off") {
    data <- data %>% mutate(
      Lights = case_when(
        TimesOnly >= 0 & TimesOnly < LightsOn ~ "Off",
        TimesOnly >= LightsOn & TimesOnly < LightsOff ~ "On",
        TimesOnly >= LightsOff ~ "Off"
      )
    )
  }
  
  data_iterator <- 1
  data_dark_list <- list()
  data_light_list <- list()
  while (data_iterator <= data_length) {
    data_singleSN <- data %>% filter(.id == SNs[data_iterator])
    data_days <- ceiling(data_singleSN$ElapsedTimeD %>% max())
    data_dark <- filter(data_singleSN, Lights == "Off")
    data_light <- filter(data_singleSN, Lights == "On")
    
    data_dark_avg <- tibble(ElapsedTimeD = c(1:data_days), SBP = c(1:data_days),
                            DBP = c(1:data_days), MAP = c(1:data_days),
                            HR = c(1:data_days), Temp = c(1:data_days),
                            Activity = c(1:data_days))
    data_light_avg <- tibble(ElapsedTimeD = c(1:data_days), SBP = c(1:data_days),
                             DBP = c(1:data_days), MAP = c(1:data_days),
                             HR = c(1:data_days), Temp = c(1:data_days),
                             Activity = c(1:data_days))
    
    days_iterator <- 1
    while (days_iterator <= data_days) {
      data_dark_avg[days_iterator,] <- data_dark %>% select(-1:-6,-Lights) %>% 
        filter(ElapsedTimeD < days_iterator & ElapsedTimeD >= days_iterator - 1) %>%
        colMeans(na.rm=TRUE)
      
      data_light_avg[days_iterator,] <- data_light %>% select(-1:-6,-Lights) %>% 
        filter(ElapsedTimeD < days_iterator & ElapsedTimeD >= days_iterator - 1) %>%
        colMeans(na.rm=TRUE)
      
      if (days_iterator == data_days) {
        colnames(data_dark_avg)[2:7] <- c(paste(SNs[data_iterator],"SBP",sep=""),
                                          paste(SNs[data_iterator],"DBP",sep=""),
                                          paste(SNs[data_iterator],"MAP",sep=""),
                                          paste(SNs[data_iterator],"Temp",sep=""),
                                          paste(SNs[data_iterator],"HR",sep=""),
                                          paste(SNs[data_iterator],
                                                "Activity",sep=""))
        data_dark_avg$ElapsedTimeD <- c(1:data_days)
        colnames(data_light_avg)[2:7] <- c(paste(SNs[data_iterator],"SBP",sep=""),
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
  return (circadian_list)}