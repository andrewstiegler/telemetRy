#' Isolate parameters from typical_day dataframe.
#'
#' @description
#' Theis function takes the output of typical_day and outputs a new
#' dataframe for the specified parameter.
#' @param data A dataframe which is the output from typical_day.
#' @param parameter Name of a parameter in the dataset, in quotes.
#' @return A dataframe with the Time columns from the input,
#' and a column for each subject appended with the parameter (i.e. SBP).
#'  @examples
#' isolate_typical(data = typical_day_output, parameter = "SBP")
#' isolate_typical(data = typical_day_output, parameter = "HR")

# isolate parameter in general
isolate_typical <- function (data, parameter) {
    param_missing <- missing(parameter)
    if (param_missing) return ("No parameter - please set parameter to isolate")
    if (sum(grep(paste(parameter),colnames(data))) == 0) {
        return ("Parameter not found in dataset")
    }
    if (colnames(data)[1] != "Time") {
        return ("First column is not Time column - wrong data?")
    }
    typical_param <- select(data, 1, grep(paste(parameter),
                                          colnames(data)))
    return (typical_param)
}
