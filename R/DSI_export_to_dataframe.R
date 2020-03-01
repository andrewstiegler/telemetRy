#' Import an Excel file created by DSI's Ponemah export function into R.
#'
#' This function takes the path to an Excel file as input and generates a
#' dataframe containing the time, several measures of elapsed time since the
#' start of the recording, and all telemetry parameters (SBP, DBP, MAP,
#' HR, Temperature, Activity)
#' @param selected_file Path to an Excel file created by Ponemah.
#' @return A dataframe with 13 columns: .id (SN), Time, TimesOnly, ElapsedTime,
#' ElapsedTimeMin, ElapsedTimeH, ElapsedTimeD, SBP, DBP, MAP, HR, Temp,
#' Activity)
#' @examples
#' DSI_export_to_dataframe(selected_file)


# DSI_export_to_dataframe function takes same input filepath of DSI exported
# Excel file as input
# Returns a dataframe with columns:
# .id (telemeter SN), Time, ElapsedTime (various units), BP parameters


DSI_export_to_dataframe <- function (selected_file) {
    filetype_check <- readxl::excel_format(selected_file)
    if (is.na(filetype_check)) {
        return("Must select Excel file")
    }

    sheet_list <- excel_sheets(selected_file)
    if (sum(grepl("Parameters", sheet_list)) == 0) {
        return("No 'Parameters' sheets. Can't import this file.")
    }

    SN_list <- 1
    SN_first_loop <- 1
    for (sheet_loop_iterator in 1:length(sheet_list)){
        if (substr(sheet_list[sheet_loop_iterator],0,1) == "P"){
            SN_list[SN_first_loop] <- substr(sheet_list[sheet_loop_iterator],
                                             20, 26)
            #Check if the SN is 6 or 7 digits
            if (substr(SN_list[SN_first_loop], 7, 7) == ")") {
                SN_list[SN_first_loop] <-
                    substr(sheet_list[sheet_loop_iterator], 20, 25)
            }
            SN_first_loop <- SN_first_loop + 1
        }
        sheet_loop_iterator<- sheet_loop_iterator + 1
    }

    import_data_list <- list()
    for (SN_iterator in 1:length(SN_list)) {
        progress(SN_iterator, length(SN_list))
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
        imported_sheet$Time <-
            excel_numeric_to_date(as.numeric(imported_sheet$Time),
                                  include_time = TRUE)

        #DSI S/N's can be 6 or 7 numbers long, we want SN to reflect this
        SN <- substr(sheet_list[SN_iterator], 20, 26)
        if(substr(SN, 7, 7) == ")")
            SN <- substr(sheet_list[SN_iterator], 20, 25)

        #Create new columns for elapsed time
        imported_sheet$ElapsedTime <- 0

        #Loop over entire recording to calculate elapsed time for each row
        for (elapsed_iterator in 2:length(imported_sheet$Time)){
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
