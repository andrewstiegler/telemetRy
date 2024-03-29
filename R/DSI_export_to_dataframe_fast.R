#' Import an Excel file created by DSI's Ponemah export function into R.
#'
#' This function takes the path to an Excel file as input and generates a
#' dataframe containing the time, several measures of elapsed time since the
#' start of the recording, and all telemetry parameters (SBP, DBP, MAP,
#' HR, Temperature, Activity)
#' @param selected_file Path to an Excel file created by Ponemah.
#' @param output_format W by default. W for wide dataframes, one row per time
#' point and one column per subject per parameter. L for long dataframes, one
#' row per time point per subject and one column per parameter.
#' @return A dataframe with 13 columns: .id (SN), Time, TimesOnly, ElapsedTime,
#' ElapsedTimeMin, ElapsedTimeH, ElapsedTimeD, SBP, DBP, MAP, HR, Temp,
#' Activity)

# DSI_export_to_dataframe function takes same input filepath of DSI exported
# Excel file as input
# Returns a dataframe with columns:
# .id (telemeter SN), Time, ElapsedTime (various units), BP parameters


DSI_export_to_dataframe_fast <- function (selected_file, output_format = "W") {
    filetype_check <- readxl::excel_format(selected_file)
    if (is.na(filetype_check)) {
        stop("Must select Excel file")
    }

    output_format <- enquo(output_format)
    if (!(!! quo_get_expr(output_format) == "W" |
          !! quo_get_expr(output_format) == "L")) {
        stop("output_format must be long (L) or wide (W)")
    }

    sheet_list <- excel_sheets(selected_file)
    if (sum(grepl("Parameters", sheet_list)) == 0) {
        stop("No 'Parameters' sheets. Can't import this file.")
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
            rename(Time = .data$`1`, SBP = .data$`9`, DBP = .data$`10`,
                   MAP = .data$`11`, HR = .data$`12`,
                   Temp = .data$`13`, Activity = .data$`16`) %>%
            tail(-3) %>% mutate_all(~ as.numeric(.))
        #Fix time from excel import
        imported_sheet$Time <-
            excel_numeric_to_date(imported_sheet$Time,
                                  include_time = TRUE)

        #DSI S/N's can be 6 or 7 numbers long, we want SN to reflect this
        SN <- substr(sheet_list[SN_iterator], 20, 26)
        if(substr(SN, 7, 7) == ")")
            SN <- substr(sheet_list[SN_iterator], 20, 25)

        #Create new columns for elapsed time
        imported_sheet$ElapsedTime <- imported_sheet$Time - imported_sheet$Time[1]

        #Loop over entire recording to calculate elapsed time for each row
        # for (elapsed_iterator in 2:length(imported_sheet$Time)){
        #     imported_sheet$ElapsedTime[elapsed_iterator] <-
        #         as.numeric(imported_sheet$Time[elapsed_iterator]) -
        #         as.numeric(imported_sheet$Time[1])
        #     elapsed_iterator <- elapsed_iterator+1
        # }

        #Convert elapsed time into only times for calculating lights on/off
        imported_sheet$TimesOnly <- as.ITime(imported_sheet$Time)
        #Rearrange columns for ease of reading
        imported_sheet <- select(imported_sheet, .data$Time, .data$TimesOnly,
                                 .data$ElapsedTime, .data$SBP, .data$DBP,
                                 .data$MAP, .data$HR, .data$Temp,
                                 .data$Activity)
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
    all_import_data_long <- rbindlist(import_data_list, idcol = TRUE) %>% rename(sn = .id)

    import_data_list_wide <- list()
    for (wide_iterator in 1:length(SN_list)){
        import_data_list_wide[[wide_iterator]] <-
            import_data_list[[wide_iterator]]
        colnames(import_data_list_wide[[wide_iterator]]) <-
            paste(SN_list[[wide_iterator]],
                  colnames(import_data_list[[wide_iterator]]), sep = "_")
        colnames(import_data_list_wide[[wide_iterator]])[1] <-
            c("Time")
        wide_iterator <- wide_iterator + 1
    }

    all_import_data_wide <- import_data_list_wide %>% reduce(left_join,
                                                             by = "Time")
    colnames(all_import_data_wide)[2:3] <- c("OnlyTime", "TimeElapsed")
    all_import_data_wide <- all_import_data_wide %>%
        select(-grep("TimesOnly", colnames(all_import_data_wide)),
               -grep("ElapsedTime", colnames(all_import_data_wide)))
    colnames(all_import_data_wide)[2:3] <- c("TimesOnly", "ElapsedTime")

    if (!! quo_get_expr(output_format) == "W") {
        return (all_import_data_wide)
    }

    if (!! quo_get_expr(output_format) == "L") {
        return (all_import_data_long)
    }

}
