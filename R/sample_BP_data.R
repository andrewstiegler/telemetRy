#' Example telemetry data from one cohort of mice implanted with blood
#' pressure telemeters.
#'
#' A dataset containing time and blood pressure telemeter parameters for 15 mice
#' over approximately 4 days.
#'
#' @name sample_BP_data
#' @docType data
#' @author Andrew Stiegler \email{andrew.stiegler@gmail.com}
#' @format A data frame with 42495 rows and 13 variables:
#' \describe{
#'   \item{.id}{Serial number of telemeter, 6 or 7 digits}
#'   \item{Time}{Time stamp of observation, POSIXct}
#'   \item{TimesOnly}{Time of day from Time column, ITime}
#'   \item{ElapsedTime}{Time elapsed since recording began, seconds}
#'   \item{SN_SBP}{Systolic blood pressure for subject SN, mmHg}
#'   \item{SN_DBP}{Diastolic blood pressure for subject SN, mmHg}
#'   \item{SN_MAP}{Mean arterial blood pressure for subject SN, mmHg}
#'   \item{SN_HR}{Heart rate for subject SN, bpm}
#'   \item{SN_Temp}{Temperaturefor subject SN, degrees C}
#'   \item{SN_Activity}{Activity score for subject SN, arbitrary units}
#' }
NULL
