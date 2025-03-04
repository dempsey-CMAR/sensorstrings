# all compile foos --------------------------------------------------------

#' Set up parameters, Errors, and Warnings for the \code{compile_**} functions
#'
#' @details A column named \code{sensor_type} is added to \code{sn_table}, with
#'   values of \code{sensor_make}. The \code{sensor_type} column is added to the
#'   compiled data (e.g., "aquameasure", "hobo", "vr2ar" vs "HOBO PRO V2" or
#'   "aquaMeasure DOT").
#'
#'   Returns Errors and Warnings if the expected files are not found on the
#'   \code{path}.
#'
#' @inheritParams ss_compile_hobo_data
#'
#' @param path File path to the folder with the aquameasure, hobo, tidbit, or
#'   vemco folder.
#'
#' @param sensor_make Make of the sensor to be compiled. Should match the name
#'   of the folder with the raw data files. Most common entries will be
#'   "aquameasure", "hobo", or "vemco".
#'
#' @return Returns a list of parameters used in the \code{compile_**} functions:
#'   final path to the folder of interest, deployment dates, vector of files in
#'   the folder, and \code{sn_table}.
#'
#' @importFrom dplyr %>% mutate select
#' @importFrom glue glue
#' @importFrom lubridate parse_date_time
#' @importFrom stringr str_detect

set_up_compile <- function(path,
                           sn_table,
                           deployment_dates,
                           sensor_make) {
  # make sure columns of sn_table are named correctly
  # log_sensor is the Logger_Model from the deployment log
  names(sn_table) <- c("log_sensor", "sensor_serial_number", "depth")
  sn_table <- sn_table %>%
    filter(str_detect(log_sensor, regex(sensor_make, ignore_case = TRUE))) %>%
    # this standardizes the sensor_type column,
    ## e.g., replaces "HOBO PRO V2" with "hobo"
    mutate(sensor_type = tolower(sensor_make))

  # extract the deployment start and end dates from deployment_dates
  dates <- extract_deployment_dates(deployment_dates)

  if (dates$start > dates$end) {
    stop("The deployment date is after the retrieval date")
  }

  # name of folder (case-insensitive)
  if (sensor_make == "VR2AR") sensor_make <- "vemco"

  # if file path already includes file names
  if (all(utils::file_test("-f", path))) {
    dat_files <- path
    excel_files <- dat_files[grep("xlsx|xls", dat_files)]
  } else{
    folder <- list.files(path) %>%
      str_extract(regex(sensor_make, ignore_case = TRUE)) %>%
      na.omit()

    if (length(folder) == 0) {
      stop("There is no folder named << ", sensor_make, " >> in path << ", path, " >>")
    }

    path <- glue("{path}/{folder}")

    # list files in the sensor folder
    dat_files <- list.files(path, all.files = FALSE, pattern = "*csv")
    # check for excel files
    excel_files <- list.files(path, all.files = FALSE, pattern = "*xlsx|xls")

    dat_files <- glue("{path}/{dat_files}")
  }

  # check for surprises in dat_files
  if (length(dat_files) == 0) {
    stop(glue("Can't find csv files in {path}"))
  }


  if (sensor_make == "vemco" && length(dat_files) > 1) {
    warning(glue("There are {length(dat_files)} csv files in {path}.
                 Expect 1 file for a typical deployment"))
  }

  if (length(dat_files) != nrow(sn_table)) {
    warning(glue("There are {length(dat_files)} csv files in {path}.
              Expected {nrow(sn_table)} files"))
  }

  if (length(excel_files) > 0) {
    warning(glue("Can't compile excel files.
    {length(excel_files)} excel files found.
    \nHINT: Please re-export in csv format."))
  }

  # return info
  list(
    path = path,
    dates = dates,
    dat_files = dat_files,
    sn_table = sn_table
  )
}


#' Add and reorder columns
#'
#' Add \code{deployment_range}, and \code{sensor_type}, \code{sensor_serial
#' number} columns. Rename \code{depth} column to \code{sensor_depth_at_low_tide_m}.
#'
#' @inheritParams ss_compile_hobo_data
#'
#' @param dat placeholder
#' @param start_date placeholder
#' @param end_date placeholder
#'
#' @return returns dat with additional columns

add_deployment_columns <- function(
    dat,
    start_date,
    end_date,
    sn_table) {
  dat %>%
    mutate(
      deployment_range = paste(
        format(start_date, "%Y-%b-%d"), "to", format(end_date, "%Y-%b-%d")
      ),
      sensor_type = sn_table$sensor_type,
      sensor_serial_number = as.numeric(sn_table$sensor_serial_number),
      sensor_depth_at_low_tide_m = sn_table$depth
    ) %>%
    select(
      deployment_range,
      contains("timestamp"),
      sensor_type,
      sensor_serial_number,
      sensor_depth_at_low_tide_m,
      chlorophyll_blue_ug_per_l = contains("blue_ug"),
      chlorophyll_red_ug_per_l = contains("red_ug"),
      dissolved_oxygen_percent_saturation = contains("percent_sat"),
      dissolved_oxygen_uncorrected_mg_per_l = contains("uncorrected_mg_per_l"),
      sensor_depth_measured_m = contains("sensor_depth_measured"),
      salinity_psu = contains("psu"),
      temperature_degree_c = contains("degree_c")
    )
}


#' Check number of rows of data file
#'
#' @param dat Data frame. An Error will be returned if there are 0 rows of data.
#'
#' @param file_name Name of file that is being checked, e.g., the csv file that
#'   was read in as \code{dat}. Used in the Error message to pinpoint the
#'   problematic file.
#'
#' @param trimmed Logical value indicating if \code{dat} has been trimmed.
#'
#' @return Returns an Error if there are no rows in \code{dat}.

check_n_rows <- function(dat, file_name, trimmed = TRUE) {
  if (nrow(dat) == 0) {
    if (isFALSE(trimmed)) {
      stop("Before trimming, there are 0 rows of data in file ", file_name)
    }

    if (isTRUE(trimmed)) {
      stop("After trimming, there are 0 rows of data in file ", file_name)
    }
  }
}


#' Convert timestamp to datetime
#'
#' @param dat Data frame with at least one column, \code{timestamp_} that has
#'   datetimes as character values.
#'
#' @details Converts the timestamp_ column to a POSIXct object. Every datetime
#'   entry must be in the same order.
#'
#' @importFrom lubridate parse_date_time

convert_timestamp_to_datetime <- function(dat) {
  date_format <- dat$timestamp_[1] # first datetime value; use to check the format

  parse_orders <- c(
    "ymd IMS p", "Ymd IMS p",
    "Ymd HM", "Ymd HMS",
    "dmY HM", "dmY HMS",
    "dmY IM p", "dmY IMS p", "Ymd"
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


#' Extract deployment dates
#'
#' @param deployment_dates Data frame with two columns; the first column holds
#'   the deployment date, and the second column holds the retrieval date.
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


#' Trim data to specified start and end dates.
#'
#' 4 hours adde to end_date to account for AST (e.g., in case the sensor was
#' retrieved after 20:00 AST, which is 00:00 UTC **The next day**)
#'
#' @param dat Data frame with at least one column. Column name must include the
#'   string "timestamp".
#'
#' @param start_date POSIXct/POSIXt value of the first good timestamp.
#'
#' @param end_date POSIXct/POSIXt value of the last good timestamp.
#'
#' @importFrom assertthat assert_that
#' @importFrom lubridate hours is.POSIXct
#' @importFrom stringr str_detect
#'
#' @return Returns dat trimmed.

trim_data <- function(dat, start_date, end_date) {
  assert_that(
    is.POSIXct(start_date),
    msg = glue("'start_date' must be of type 'POSIXct', not {class(start_date)}")
  )
  assert_that(
    is.POSIXct(end_date),
    msg = glue("'end_date' must be of type 'POSIXct', not {class(end_date)}")
  )
  # assert_posixct(start_date)
  # assert_posixct(end_date)

  ind <- colnames(dat)[which(str_detect(colnames(dat), "timestamp"))]

  dat %>%
    filter(
      .data[[ind[[1]]]] >= start_date,
      .data[[ind[[1]]]] <= (end_date + hours(4))
    )
}


# aqumeasure --------------------------------------------------------------

#' Extract the timezone of aquameasure timestamps
#'
#' @inheritParams extract_aquameasure_vars
#'
#' @return Returns a character string of the timezone indicated in the Timestamp
#'   column.
#'
#' @importFrom stringr str_detect str_split

extract_aquameasure_tz <- function(am_colnames) {
  tz_name <- am_colnames[which(str_detect(am_colnames, "stamp"))]

  x <- str_split(tz_name, pattern = "\\(")

  y <- str_split(x[[1]][2], "\\)")

  tolower(y[[1]][1])
}


#' Extract the variables included in aquameasure file from the column names
#'
#' @param am_colnames Column names of aquameasure data file.
#'
#' @return Returns a vector of the variables included in the file.

extract_aquameasure_vars <- function(am_colnames) {

  temp <- ifelse("Temperature" %in% am_colnames, "Temperature", NA)
  DO <- ifelse("Dissolved Oxygen" %in% am_colnames, "Dissolved Oxygen", NA)
  sal <- ifelse("Salinity" %in% am_colnames, "Salinity", NA)
  sensor_depth <- ifelse("Device Depth" %in% am_colnames, "Device Depth", NA)
  chl_blue <- ifelse("Chlorophyll Blue" %in% am_colnames, "Chlorophyll Blue", NA)
  chl_red <- ifelse("Chlorophyll Red" %in% am_colnames, "Chlorophyll Red", NA)


  # create vector of the variables in this file by removing NA
  vars <- c(temp, DO, sal, sensor_depth, chl_blue, chl_red)
  vars[which(!is.na(vars))]
}


# HOBO --------------------------------------------------------------------

#' Extract hobo serial number from the data file
#'
#' @param hobo_colnames Column names of the hobo file, as imported by
#'   \code{ss_read_hobo_data()}.
#'
#' @return Returns the hobo serial number.
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
      glue("Hobo file LOGR S/N ({LOGGER_SN}) does not match SEN S/N ({SENSOR_SN})")
    )
  }
}


#' Extract units from column names of hobo data
#'
#' @param hobo_dat Data as read in by \code{ss_read_hobo_data()}.
#'
#' @return Returns a tibble of \code{variable} and \code{units} found in
#'   \code{hobo_dat}. Units are mg_per_l for dissolved oxygen and degree_c for
#'   temperature.
#' @importFrom dplyr %>% contains mutate select
#' @importFrom stringr str_replace str_remove
#' @importFrom tidyr separate

extract_hobo_units <- function(hobo_dat) {
  hobo_dat %>%
    select(
      contains("Date"), contains("Temp"), contains("DO", ignore.case = FALSE)
    ) %>%
    colnames() %>%
    data.frame() %>%
    separate(col = ".", into = c("variable", "units"), sep = ", ", extra = "drop") %>%
    separate(col = "units", into = c("units", NA), sep = " \\(", fill = "right") %>%
    mutate(
      units = str_replace(units, pattern = "GMT", replacement = "utc"),
      units = str_remove(units, pattern = "\\+00:00"),
      units = str_replace(units, pattern = "mg/L", replacement = "mg_per_l"),
      units = str_replace(units, pattern = "\u00B0C", replacement = "degree_c")
    )
}

#' Glue variable name and units to create column names
#'
#' @param unit_table Data frame including columns \code{variable} and
#'   \code{units}, as returned from \code{extract_hobo_units()}.
#'
#' @return Data frame with column names in the form \code{variable_units}.
#'
#' @importFrom dplyr %>% arrange mutate
#' @importFrom stringr str_detect str_replace
#' @importFrom glue glue

make_column_names <- function(unit_table) {
  new_names <- unit_table %>%
    mutate(
      variable = str_replace(
        variable,
        pattern = "Date Time", replacement = "timestamp_"
      ),
      variable = str_replace(
        variable,
        pattern = "DO conc", replacement = "dissolved_oxygen_uncorrected_"
      ),
      variable = str_replace(
        variable,
        pattern = "Temp", replacement = "temperature_"
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


# vemco -------------------------------------------------------------------

#' Extract the timezone of vemco timestamps
#'
#' @param dat_colnames Column names of the vemco file.
#'
#' @return Returns a character string of the timezone indicated in the Timestamp
#'   column.
#'
#' @importFrom stringr str_detect str_split

extract_vemco_tz <- function(dat_colnames) {
  tz_name <- dat_colnames[which(str_detect(dat_colnames, "Time"))]

  x <- str_split(tz_name, pattern = " ")

  tolower(gsub("[()]", "", x[[1]][4]))
}
