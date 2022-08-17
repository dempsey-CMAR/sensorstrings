#' convert_timestamp_to_datetime()
#'
#' @param dat Data.frame with column \code{timestamp_} that has timestamps as
#'   character values.
#'
#' @details Convert the timestamp_ column to a POSIXct object.
#'
#' @importFrom lubridate parse_date_time

convert_timestamp_to_datetime <- function(dat) {

  date_format <- dat$timestamp_[1] # first datetime value; use to check the format

  parse_orders <- c(
    "ymd IMS p", "Ymd IMS p",
    "Ymd HM", "Ymd HMS",
    "dmY HM", "dmY HMS",
    "dmY IM p", "dmY IMS p"
  )

  check_date <- suppressWarnings(
    parse_date_time(date_format, orders = parse_orders)
  )

  if (!is.na(check_date)) {
    dat <- dat %>%
      mutate(
        timestamp_ = lubridate::parse_date_time(timestamp_, orders = parse_orders)
      )
  } else {

    # Error message if the date format is incorrect
    stop(paste0("Can't parse date in format ", date_format))

  }

  dat
}


#' Extract HOBO serial number from the data file
#'
#' @param hobo_colnames Column names of the HOBO file, as imported by
#'   \code{ss_read_hobo_data()}.
#'
#' @return Returns the HOBO serial number.
#'
#' @importFrom glue glue
#' @importFrom stringr str_detect str_remove str_split

extract_hobo_sn <- function(hobo_colnames) {

  SN <- hobo_colnames[str_detect(hobo_colnames, pattern = "Temp")]
  SN <- str_split(SN, pattern = ", ")

  LOGGER_SN <- str_split(SN[[1]][2], pattern = ": ")
  LOGGER_SN <- LOGGER_SN[[1]][2]

  SENSOR_SN <- str_split(SN[[1]][3], pattern = ": ")
  SENSOR_SN <- str_remove(SENSOR_SN[[1]][2], pattern = "\\)")

  if (LOGGER_SN == SENSOR_SN) {
    as.numeric(SENSOR_SN)
  } else {
    stop(
      glue("HOBO file LOGR S/N ({LOGGER_SN}) does not match SEN S/N ({SENSOR_SN})")
    )
  }
}


#' Extract units from column names of hobo data
#'
#' @param hobo_dat Data as read in by \code{ss_read_hobo_data()}.
#'
#' @return Returns a tibble of \code{variable} and \code{units} found in
#'   \code{hobo_dat}. Units are mg_per_L for dissolved oxygen and degree_C for
#'   temperature.
#' @importFrom dplyr %>% contains mutate select
#' @importFrom stringr str_replace str_remove
#' @importFrom tidyr separate

extract_hobo_units <- function(hobo_dat) {

  hobo_dat %>%
    select(contains("Date"), contains("Temp"), contains("DO")) %>%
    colnames() %>%
    data.frame() %>%
    separate(col = ".", into = c("variable", "units"), sep = ", ", extra = "drop") %>%
    separate(col = "units", into = c("units", NA), sep = " \\(", fill = "right") %>%
    mutate(
      units = str_replace(units, pattern = "GMT", replacement = "utc"),
      units = str_remove(units, pattern = "\\+00:00"),
      units = str_replace(units, pattern = "mg/L", replacement = "mg_per_L"),
      units = str_replace(units, pattern = "\u00B0C", replacement = "degree_C")
    )
}


#' Extract the variables included in aquameasure file from the column names
#'
#' @param am_colnames Column names of aquameasure data file.
#'
#' @return Returns a vector of the variables included in the file.

extract_aquameasure_vars <- function(am_colnames) {

  ## check colnames of dat.i for "Temperature", "Dissolved Oxygen", and "Salinity"
  temp <- ifelse("Temperature" %in% am_colnames, "Temperature", NA)
  DO <- ifelse("Dissolved Oxygen" %in% am_colnames, "Dissolved Oxygen", NA)
  sal <- ifelse("Salinity" %in% am_colnames, "Salinity", NA)

  # create vector of the variables in this file by removing NA
  vars <- c(temp, DO, sal)
  vars[which(!is.na(vars))]
}


#' Glue variable name and units to create column names
#'
#' @param unit_table Data.frame including columns \code{variable} and
#'   \code{units}, as returned from \code{extract_hobo_units()}.
#'
#' @return Data.frame with column names in the form \code{variable_units}.
#'
#' @importFrom dplyr %>% arrange mutate
#' @importFrom stringr str_detect str_replace
#' @importFrom glue glue
#'
#' @export

make_column_names <- function(unit_table) {

  new_names <- unit_table %>%
    mutate(
      variable = str_replace(
        variable, pattern = "Date Time", replacement = "timestamp_"
      ),
      variable = str_replace(
        variable, pattern = "DO conc", replacement = "dissolved_oxygen_"
      ),
      variable = str_replace(
        variable, pattern = "Temp", replacement = "temperature_"
      ),
      col_name = glue("{variable}{units}")
    )

  # make ordered factor so rows will always be in this order
  ## timestamp, dissolved oxygen, temperature
  ## this is important because the columns will be named in this order
  f_levels <- c(
    new_names[str_detect(new_names$col_name, "timestamp"), ]$col_name,
    new_names[str_detect(new_names$col_name, "dissolved_oxygen"), ]$col_name,
    new_names[str_detect(new_names$col_name, "temperature"), ]$col_name
  )

  new_names %>%
    mutate(col_name = ordered(col_name, levels = f_levels)) %>%
    arrange(col_name) %>%
    mutate(col_name = as.character(col_name))
}

#' @title Extracts the extension of a file name
#'
#' @details Extracts the file extension from a character string using //. as the
#'  separator.
#'
#' @param file_name Character string of a file name. Must only include one ".",
#'  which is used as the separator.
#'
#' @importFrom tidyr separate

extract_file_extension <- function(file_name){

  extension <- file_name %>%
    data.frame() %>%
    separate(col = 1, into = c(NA, "EXT"), sep = "\\.")

  extension$EXT
}


#' Extract deployment dates
#' @param deployment_dates Data.frame with start and end dates of the deployment
#'   in the form yyyy-mm-dd. Two columns: \code{START} and \code{END}.
#'
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
