#' @title Extracts the extension of a file name
#'
#' @details Extracts the file exension from a character string using //. as the
#'  separator.
#'
#' @param file_name Character string of a file name. Must only include one ".",
#'  which is used as the separator.
#'
#' @export
#'
#' @importFrom tidyr separate

# extract_file_extension <- function(file_name){
#
#   extension <- file_name %>%
#     data.frame() %>%
#     separate(col = 1, into = c(NA, "EXT"), sep = "\\.")
#
#   extension$EXT
# }

# convert_timestamp_to_datetime() -----------------------------------------

#' @importFrom janitor convert_to_datetime
#' @importFrom lubridate parse_date_time

# function to convert the timestamp to a POSIXct object
# if the date can be converted to class numeric, then it was stored as a number in Excel
## and we have to use janitor::convert_to_datetime to convert to POSIXct.
# Otherwise the date should be a character string that can be converted to POSIXct using
## lubridate::parse_date_time()

convert_timestamp_to_datetime <- function(sensor.data) {
  date_format <- sensor.data$TIMESTAMP[1] # first datetime value; use to check the format

  # if saved as a number in Excel
  if (!is.na(suppressWarnings(as.numeric(date_format)))) {
    sensor.data <- sensor.data %>%
      mutate(TIMESTAMP = janitor::convert_to_datetime(as.numeric(TIMESTAMP)))
  } else {

    # if saved as a character string in Excel
    parse.orders <- c(
      "ymd IMS p", "Ymd IMS p",
      "Ymd HM", "Ymd HMS",
      "dmY HM", "dmY HMS",
      "dmY IM p", "dmY IMS p"
    )

    if (!is.na(suppressWarnings(parse_date_time(date_format, orders = parse.orders)))) {
      sensor.data <- sensor.data %>%
        mutate(TIMESTAMP = lubridate::parse_date_time(TIMESTAMP, orders = parse.orders))
    } else {

      # Error message if the date format is incorrect
      stop("Timestamp is not in a format recognized by the strings package.
           See help files for more information.")
    }
  }

  sensor.data
}



#' Extract HOBO serial number from the data file
#'
#' @param hobo_colnames Column names of the HOBO file, as imported by
#'   \code{ss_read_hobo_data}.
#'
#' @return Returns a character string of the HOBO serial number.
#' @export
#'

extract_hobo_sn <- function(hobo_colnames) {
  SN <- hobo_colnames[str_detect(hobo_colnames, pattern = "Temp")]
  SN <- str_split(SN, pattern = ", ")

  LOGGER_SN <- str_split(SN[[1]][2], pattern = ": ")
  LOGGER_SN <- LOGGER_SN[[1]][2]

  SENSOR_SN <- str_split(SN[[1]][3], pattern = ": ")
  SENSOR_SN <- str_remove(SENSOR_SN[[1]][2], pattern = "\\)")

  if (LOGGER_SN == SENSOR_SN) {
    SENSOR_SN
  } else {


    # check this
    stop(
      glue(
        "In HOBO file LOGR S/N ({LOGGER_SN$LOGGER_SN}) does not match SEN S/N ({SENSOR_SN$SENSOR_SN})"
      )
    )
  }
}


#' Extract measurement time zone from the data file
#'
#' @param hobo_colnames Column names of the HOBO file, as imported by
#'   \code{ss_read_hobo_data}.
#'
#' @return Returns a character string of the HOBO serial number.
#' @export
#'

extract_hobo_tz <- function(hobo_colnames) {
  TZ <- hobo_colnames[str_detect(hobo_colnames, pattern = "Date")]
  TZ <- str_split(TZ, pattern = ", ")

  TZ <- TZ[[1]][2]

  if (TZ == "GMT+00:00") {
    TZ
  } else {
    TZ

    warning(glue("Timezone of HOBO file is {TZ}"))
  }
}



# extract_deployment_dates() ----------------------------------------------

# function to convert deployment and retrieval dates to datetimes
# option to trim data to these dates in the compile_xx_data() functions

# deployment.dates is a dataframe with two columns: start.date and end.date
# there should be one observation in each column
# each observation must be a Date object

# returns a dataframe with 1 observation in two columns: start_date and end_date
# start_date holds a datetime for the start of the deployment (with time of 00:00:00)
# end_date holds a datetime for the end of the deployment (with time of 23:59:59)


#' @importFrom tidyr separate
#' @importFrom lubridate as_datetime

extract_deployment_dates <- function(deployment_dates) {

  # name deployment.dates
  names(deployment_dates) <- c("start_date", "end_date")

  # paste date and time and convert to a datetime object
  start_date <- as_datetime(paste(deployment_dates$start_date, "00:00:00"))
  end_date <- as_datetime(paste(deployment_dates$end_date, "23:59:59"))

  # return start and end datetimes
  data.frame(start = start_date, end = end_date)
}
