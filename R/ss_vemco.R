#' @title Import data from Vemco sensors
#'
#' @details The Vemco data must be saved in csv format.
#'
#' @inheritParams ss_read_hobo_data
#'
#' @param path File path to the vemco folder.
#'
#' @return Returns a data frame of Vemco data, with the same columns as in the
#'   original file.
#'
#' @author Danielle Dempsey
#'
#' @importFrom data.table fread
#' @importFrom dplyr %>%  as_tibble
#' @importFrom janitor convert_to_datetime
#' @importFrom lubridate parse_date_time
#' @importFrom readxl read_excel
#' @importFrom stringr str_glue
#'
#' @export


ss_read_vemco_data <- function(path, file_name) {
  assert_that(has_extension(file_name, "csv"))

  # finish path
  path <- file.path(str_glue("{path}/{file_name}"))

  data.table::fread(
    path,
    header = TRUE,
    data.table = FALSE,
    #encoding = "UTF-8",
    na.strings = ""
  )
}


#' Format temperature data from Vemco deployment
#'
#' @description Compiles and formats temperature and seawater depth data from
#'   VR2AR sensors.
#'
#' @details The raw Vemco data must be saved in a folder named Vemco in csv
#'   format. Folder name is not case-sensitive.
#'
#'   If there are "Temperature" entries in the Description column, these will be
#'   extracted and compiled. If there are no "Temperature" entries, but there
#'   are "Average temperature" entries, these will be extracted and compiled.
#'   Otherwise, the function will stop with an error message.
#'
#' @inheritParams ss_compile_hobo_data
#'
#' @param path File path to the Vemco folder. This folder should have one csv
#'   file that was extracted using Vue software. Other file types in the folder
#'   will be ignored.
#'
#' @return Returns a tibble with the data compiled from the file in path/vemco.
#'
#' @family compile
#' @author Danielle Dempsey
#'
#' @importFrom dplyr %>% case_when mutate select contains tibble
#' @export

ss_compile_vemco_data <- function(path,
                                  sn_table,
                                  deployment_dates,
                                  trim = TRUE){
  # set up & check for errors
  setup <- set_up_compile(
    path = path,
    sn_table = sn_table,
    deployment_dates = deployment_dates,
    sensor_make = "VR2AR"
  )

  path = setup$path

  sn_table <- setup$sn_table

  start_date <- setup$dates$start
  end_date <- setup$dates$end

  dat_files <- setup$dat_files

  # Extract metadata --------------------------------------------------------
  dat <- ss_read_vemco_data(path, dat_files)

  dat_colnames <- colnames(dat)

  # check timezone
  date_tz <- extract_vemco_tz(dat_colnames)

  if (date_tz != "utc") {
    message(glue("Timestamp in file {dat_files} is in timezone: {date_tz}."))
  }

   # sensor and serial number
  sensor_serial <- unique(dat$Receiver)

  if (sensor_serial != sn_table$sensor_serial) {
    stop(glue("Serial number in sn_table ({sensor_serial}) does not match serial
              number in file {dat_files} ({sn_table$sensor_serial})"))
  }

  # Format data -------------------------------------------------------------

  if("Temperature" %in% unique(dat$Description)) {
    temperature_var = "Temperature"
  } else if("Average temperature" %in% unique(dat$Description)){
    temperature_var = "Average temperature"
  } else stop("Could not find Temperature or Average temperature in vemco_dat. Check file.")


  if("Date and Time (UTC)" %in% dat_colnames & "Date/Time" %in% dat_colnames){
    warning("There are two datetime columns in the Vemco data")
  }

  vars <- c("Seawater depth", "Temperature", "Average temperature")

  # extract sensor depth
  dat <- dat %>%
    select(
      timestamp_ = contains("Time"),
      Description,
      Data,
      Units
    ) %>%
    filter(Description %in% vars) %>%
    mutate(
      Description = dplyr::case_when(
        Description == "Seawater depth" ~ "sensor_depth_measured",
        Description == "Temperature" ~ "temperature",
        Description == "Average temperature" ~ "temperature"
      ),
      Units = if_else(
        Units == "\u00B0C" |           # if csv is saved with ANSI encoding
          Units == "\u00C2\u00B0C",     # if csv is saved with UTF-8 encoding
        "degree_C", Units
        ),
      Description = paste(Description, Units, sep = "_"),
      Data = as.numeric(Data)
    ) %>%
    tidyr::pivot_wider(
      id_cols = "timestamp_",
      names_from = "Description", values_from = Data
    ) %>%
    convert_timestamp_to_datetime()


  check_n_rows(dat, file_name = dat_files, trimmed = FALSE)

  # trim to the dates in deployment_dates
  if (isTRUE(trim)) dat <- trim_data(dat, start_date, end_date)

  check_n_rows(dat, file_name = dat_files, trimmed = trim)


  colnames(dat)[which(str_detect(colnames(dat), "timestamp"))] <- paste0("timestamp_", date_tz)

 # browser()

  # # add other useful columns and re-order ------------------------------------------------
  dat1 <- dat %>%
    mutate(
      deployment_range = paste(
        format(start_date, "%Y-%b-%d"), "to", format(end_date, "%Y-%b-%d")
      ),
      sensor = sensor_serial,
      sensor_depth_at_low_tide_m = sn_table$depth
    )

  dat2 <- dat1 %>%
    select(
      deployment_range,
      #timestamp_utc,
      contains("timestamp"),
      sensor,
      sensor_depth_at_low_tide_m
      #temperature_degree_C
      # dplyr::contains("temperature"),
      # dplyr::contains("sensor_depth_measured")
    )

  message(paste("Vemco data compiled:", temperature_var))

  tibble(dat2)

}
