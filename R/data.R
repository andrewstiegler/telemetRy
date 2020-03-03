#' Example telemetry data from one cohort of mice implanted with blood
#' pressure telemeters.
#'
#' A dataset containing time and blood pressure telemeter parameters for 15 mice
#' over approximately 4 days.
#'
#' @format A data frame with 42495 rows and 13 variables:
#' \describe{
#'   \item{.id}{Serial number of telemeter, 6 or 7 digits}
#'   \item{Time}{Time stamp of observation, POSIXct}
#'   \item{TimesOnly}{Time of day from Time column, ITime}
#'   \item{ElapsedTime}{Time elapsed since recording began, seconds}
#'   \item{ElapsedTimeMin}{Time elapsed since recording began, minutes}
#'   \item{ElapsedTimeH}{Time elapsed since recording began, hours}
#'   \item{ElapsedTimeD}{Time elapsed since recording began, days}
#'   \item{SBP}{Systolic blood pressure, mmHg}
#'   \item{DBP}{Diastolic blood pressure, mmHg}
#'   \item{MAP}{Mean arterial blood pressure, mmHg}
#'   \item{HR}{Heart rate, bpm}
#'   \item{Temp}{Temperature, degrees C}
#'   \item{Activity}{Activity score, arbitrary units}
#' }
#' @source \url{http://www.diamondse.info/}
