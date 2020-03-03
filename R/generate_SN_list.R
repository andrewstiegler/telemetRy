#' Import an Excel file created by DSI's Ponemah export function into R.
#'
#' This function takes the path to an Excel file as input and generates a vector
#' containing all serial numbers found in the Excel file.
#'
#' @param selected_file Path to an Excel file created by Ponemah.
#' @return A character vector containing all serial numbers in the Ponemah export.

# generate_SN_list function takes filepath of DSI exported Excel file as input
# Returns list of telemeter SNs as vector

#' @import readxl
#' @import dplyr
#' @import magrittr
#' @export generate_SN_list
generate_SN_list <- function(selected_file) {
    filetype_check <- readxl::excel_format(selected_file)
    if (filetype_check != "xlsx") {
        stop("Must select Excel file")
    }

    sheet_list <- excel_sheets(selected_file)
    if (!"Parameters" %in% sheet_list) {
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
    return (SN_list)
}
