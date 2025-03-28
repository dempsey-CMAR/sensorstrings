#' @title Import data from aquameasure csv files
#'
#' @details The aquameasure data must be saved in csv format.
#'
#' @inheritParams ss_read_hobo_data
#'
#' @param path File path to the aquameasure folder, or full path to the data
#'   file including file name and extension.
#'
#' @return Returns a data frame of aquameasure data, with the same columns as in
#'   the original file.
#'
#' @author Danielle Dempsey
#'
#' @importFrom assertthat assert_that has_extension
#' @importFrom data.table fread
#' @importFrom stringr str_glue
#'
#' @export

ss_read_aquameasure_data <- function(path, file_name = NULL) {

  # finish path if needed
  if (isFALSE(utils::file_test("-f", path))) {
    path <- file.path(str_glue("{path}/{file_name}"))
  }

  assert_that(has_extension(path, "csv"))

  data.table::fread(
    path,
    header = TRUE, data.table = FALSE, na.strings = "", # might need to add ERR to na.strings
    fill = TRUE
  )
}


#' Compile data from aquameasure sensors
#'
#' @description Compile and format temperature, dissolved oxygen, salinity,
#'   and/or device depth data from aquameasure sensors.
#'
#' @details The raw aquameasure data must be saved in a folder named aquameasure
#'   in csv format. Folder name is not case-sensitive.
#'
#'   Rows with \code{undefined} and \code{... (time not set)} values in the
#'   \code{Timestamp(UTC)} column are filtered out.
#'
#'   The timestamp columns must be in the order "ymd IMS p", "Ymd IMS p", "Ymd
#'   HM", "Ymd HMS", "dmY HM", or "dmY HMS".
#'
#'   "ERR" values are converted to \code{-111} (to distinguish from sensor error
#'   value of -101.5).
#'
#' @inheritParams ss_compile_hobo_data
#'
#' @return Returns a tibble with the compiled data from each of the aquameasure
#'   files saved in path/aquameasure.
#'
#' @family compile
#' @author Danielle Dempsey
#'
#' @importFrom dplyr %>% across all_of any_of bind_rows distinct group_by mutate
#'   n select slice summarise tibble
#' @importFrom glue glue
#' @importFrom lubridate parse_date_time
#' @importFrom stringr str_detect str_replace
#' @importFrom tidyr separate pivot_wider
#'
#' @export

ss_compile_aquameasure_data <- function(path,
                                        sn_table,
                                        deployment_dates,
                                        trim = TRUE) {
  # set up & check for errors
  setup <- set_up_compile(
    path = path,
    sn_table = sn_table,
    deployment_dates = deployment_dates,
    sensor_make = "aquameasure"
  )

  path <- setup$path
  dat_files <- setup$dat_files
  sn_table <- setup$sn_table

  start_date <- setup$dates$start
  end_date <- setup$dates$end

  # initialize list for storing the output
  am_dat <- list(NULL)

  # Import data -------------------------------------------------------------

  # loop over each aM file
  for (i in seq_along(dat_files)) {
    #file_name <- dat_files[i]

    file_i <- dat_files[i]
    file_name <- sub(".csv", "", sub(".*/", "", file_i, perl = TRUE))

    am_i <- ss_read_aquameasure_data(file_i) %>%
      filter(!str_detect(`Record Type`, "aquaMeasure-"))

    am_colnames <- colnames(am_i)

    if("Record Number" %in% am_colnames) am_i <- select(am_i, -`Record Number`)

    # sn and timezone checks --------------------------------------------------

    # serial number
    sn_i <- am_i %>%
      distinct(Sensor) %>%
      separate(Sensor, into = c("sensor", "serial number"), sep = "-")
    sn_i <- sn_i$`serial number`

    # check timezone
    date_tz <- extract_aquameasure_tz(am_colnames)

    if (length(sn_i) > 1) {
      # replace blank serial numbers with phrase so that it will show up in message
      sn_i <- str_replace(sn_i, " ", "blank")

      warning(
        "Multiple serial numbers found in file ",
        file_name, ": ", paste(sn_i, collapse = " ")
      )
    }

    # assume first serial number is correct
    sn_i <- sn_i[1]

    # if the serial number doesn't match any of the entries in sn_table
    if (!(sn_i %in% sn_table$sensor_serial_number)) {
      stop(glue("Serial number {sn_i[1]} does not match any serial numbers in sn_table"))
    }

    if (date_tz != "utc") {
      message(glue("Timestamp in file {file_name} is in timezone: {date_tz}."))
    }

    # Clean and format data ---------------------------------------------------
    if ("Temperature" %in% am_colnames && "Temp(Water)" %in% am_colnames) {
      warning("There is a column named Temperature and a column named Temp(Water) in", file_name)
    }

    # Re-name the "Temp(Water)" column to "Temperature"
    if (!("Temperature" %in% am_colnames) & "Temp(Water)" %in% am_colnames) {
      am_i <- am_i %>% rename(Temperature = `Temp(Water)`)
    }

    # re-format and add other columns of interest --------------------------------------------------------

    # use serial number to identify the depth from sn_table
    sensor_info_i <- dplyr::filter(sn_table, sensor_serial_number == sn_i)

    # variables to process
    vars <- extract_aquameasure_vars(colnames(am_i))

    am_i <- am_i %>%
      select(
        timestamp_ = contains("stamp"),
        `Record Type`,
        all_of(vars)
      ) %>%
      filter(
        !str_detect(timestamp_, "after"),
        !str_detect(timestamp_, "undefined"),
        `Record Type` %in% vars
      ) %>%
      convert_timestamp_to_datetime()

    # check there are more than 0 rows in am_i
    check_n_rows(am_i, file_name = file_name, trimmed = FALSE)

    # trim to the dates in deployment_dates
    if (isTRUE(trim)) am_i <- trim_data(am_i, start_date, end_date)

    check_n_rows(am_i, file_name = file_name, trimmed = trim)

    # find if any duplicate timestamps
    bad_ts <- am_i %>%
      group_by(timestamp_) %>%
      summarise(n = n()) %>%
      filter(n > length(vars))

    if (nrow(bad_ts) > 0) {
      message(
        "Duplicate timestamp(s) found and removed from aquameasure ",
        sn_i, ": ",
        paste(bad_ts$timestamp_, collapse = ", ")
      )
    }

    # remove duplicate timestamps and pivot wider
    am_i <- am_i %>%
      filter(!(timestamp_ %in% bad_ts$timestamp_)) %>%
      tidyr::pivot_wider(
        id_cols = "timestamp_",
        names_from = "Record Type", values_from = dplyr::all_of(vars)
      ) %>%
      select(
        timestamp_,
        do_percent_saturation = contains("Dissolved Oxygen_Dissolved Oxygen"),
        temperature_degree_c = contains("Temperature_Temperature"),
        salinity_psu = contains("Salinity_Salinity"),
        sensor_depth_measured_m = contains("Device Depth_Device Depth"),
        chlorophyll_blue_ug_per_l = contains("Chlorophyll Blue_Chlorophyll Blue"),
        chlorophyll_red_ug_per_l = contains("Chlorophyll Red_Chlorophyll Red")
      ) %>%
      add_deployment_columns(start_date, end_date, sensor_info_i)

    colnames(am_i)[which(str_detect(colnames(am_i), "timestamp"))] <- paste0("timestamp_", date_tz)


    # convert ERR to -111 so that column can be saved as numeric --------------
    vars_ss <- c(
      "chlorophyll_blue_ug_per_l",
      "chlorophyll_red_ug_per_l",
      "dissolved_oxygen_percent_saturation",
      "salinity_psu",
      "sensor_depth_measured_m",
      "temperature_degree_c"
    )

    am_i <- am_i %>%
      mutate(
        across(.cols = any_of(vars_ss),
               .fns = ~str_replace_all(.x, pattern = "ERR", replacement = "-111")),

        across(.cols = any_of(vars_ss), .fns = ~as.numeric(.x))
      )

    am_dat[[i]] <- am_i
  } # end loop over files

  am_out <- am_dat %>%
    map_df(bind_rows)

  message("aquameasure data compiled")

  tibble(am_out)
}
