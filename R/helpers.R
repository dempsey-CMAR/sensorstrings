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


ss_extract_hobo_units <- function(hobo_dat) {

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




#' temporary title
#'
#' @param unit_table column variable and units
#'
#' @return dataframe with col names
#'
#' @importFrom dplyr %>% arrange mutate
#' @importFrom stringr str_detect str_replace
#' @importFrom glue glue
#'
#' @export
#'
ss_make_column_names <- function(unit_table) {

  # try to find a better way to do this
  # case_when???
  new_names <- unit_table %>%
    mutate(
      variable = str_replace(variable, pattern = "Date Time", replacement = "timestamp_"),
      variable = str_replace(variable, pattern = "DO conc", replacement = "dissolved_oxygen_"),
      variable = str_replace(variable, pattern = "Temp", replacement = "temperature_"),
      col_name = glue("{variable}{units}")
    ) #%>%
  # arrange(col_name)
  #

  # make ordered factor so rows will always be i the order
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
#' @details Extracts the file extension from a character string using //. as the
#'  separator.
#' @param file_name Character string of a file name. Must only include one ".",
#'  which is used as the seprator.
#'
#' @importFrom tidyr separate

extract_file_extension <- function(file_name){

  extension <- file_name %>%
    data.frame() %>%
    separate(col = 1, into = c(NA, "EXT"), sep = "\\.")

  extension$EXT
}





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
