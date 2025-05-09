#' @title Compile and format data from hobo pH sensors (MX2501)
#'
#' @details Exported data must be saved in a folder named hobo_ph in csv format.
#'
#'   All of the csv files in the hobo_p h folder will be compiled.
#'
#'   The timestamp columns must be in the order "ymd IMS p", "Ymd IMS p", "Ymd
#'   HM", "Ymd HMS", "dmY HM", or "dmY HMS".
#'
#' @param path File path to the hobo_ph folder.
#'
#' @param sn_table A data frame with three columns: \code{sensor_type},
#'   \code{sensor_serial_number}, \code{depth}.
#'
#' @param deployment_dates A data frame with two columns. The first column holds
#'   the deployment date (a Date object, POSIXct object, or character string in
#'   the order year, month, day), and the second column holds the retrieval date
#'   (a Date object, POSIXct object, or character string in the order year,
#'   month, day).
#'
#' @param tz_check Logical argument indicating whether to check if the timestamp
#'   column is in utc. If \code{TRUE}, a warning will be printed if the timezone
#'   is NOT utc.
#'
#' @param trim Logical value indicating whether to trim the data to the dates
#'   specified in \code{deployment_dates}. (Note: four hours are added to the
#'   retrieval date to account for AST, e.g., in case the sensor was retrieved
#'   after 20:00 AST, which is 00:00 UTC the next day.) Default is \code{trim =
#'   TRUE}.
#'
#' @return Returns a tibble with the data compiled from each of the hobo or
#'   tidbit sensors.
#'
#' @family compile
#'
#' @author Danielle Dempsey
#'
#' @importFrom dplyr %>% contains everything filter if_all mutate rename select
#'   tibble
#' @importFrom lubridate hours
#' @importFrom purrr map_df
#' @importFrom stats na.omit
#' @importFrom stringr regex str_extract str_remove
#'
#' @export

ss_compile_hobo_ph_data <- function(path,
                                 sn_table,
                                 deployment_dates,
                                 tz_check = FALSE,
                                 trim = TRUE) {
  # set up & check for errors
  setup <- set_up_compile(
    path = path,
    sn_table = sn_table,
    deployment_dates = deployment_dates,
    sensor_make = "ph"
  )

  path <- setup$path
  dat_files <- setup$dat_files
  sn_table <- setup$sn_table

  start_date <- setup$dates$start
  end_date <- setup$dates$end

  # initialize list for storing the output
  hobo_dat <- list(NULL)

  # loop over each HOBO file
  for (i in seq_along(dat_files)) {
    # Import Data -------------------------------------------------------------

    # if(!is.null(file_name)) file_name <- dat_files[i]
    file_i <- dat_files[i]
    file_name <- sub(".csv", "", sub(".*/", "", file_i, perl = TRUE))

    hobo_i <- ss_read_hobo_data(file_i) %>%
      # to avoid deprecation Warning from GitHub Actions check
      filter(if_all(everything(), ~ !grepl("Logged", .)))

    # extract units and make column names
    hobo_units <- extract_hobo_ph_units(hobo_i)
    new_col_names <- make_column_names(hobo_units)

    # sn and timezone checks --------------------------------------------------
    sn_i <- str_split(file_name, " |_",)[[1]][1]

    tz_i <- filter(hobo_units, str_detect(variable, pattern = "Date"))

    # is this file in the sn_table
    if (!(sn_i %in% sn_table$sensor_serial_number)) {
      stop(paste0("Serial number ", sn_i[1], " does not match any serial numbers in sn_table"))
    }

    # is the time zone set to GMT+00:00?
    if(isTRUE(tz_check)) {
      if (tz_i$units != "utc") {
        warning(paste0("The timezone of file ", file_name,
                       " is not UTC.\nTimezone: ", tz_i$units))
      }
    }

    # Select and add columns of interest ----------------------------------------------
    hobo_i <- hobo_i %>%
      select(contains("Date"), contains("pH"), contains("Temp")) %>%
      rename(timestamp_ = 1) %>%
      mutate(timestamp_ = parse_date_time(timestamp_, orders = "mdy HMS")) %>%
      convert_timestamp_to_datetime()

    colnames(hobo_i) <- new_col_names$col_name

    # add other useful columns and re-order ------------------------------------------------

    # use serial number to identify the variable and depth (from sn_table)
    sensor_info_i <- dplyr::filter(sn_table, sensor_serial_number == sn_i)

    hobo_i <- hobo_i %>%
      add_deployment_columns(start_date, end_date, sn_table = sensor_info_i)

    check_n_rows(hobo_i, file_name = file_name, trimmed = FALSE)

    # trim to the dates in deployment_dates
    if (isTRUE(trim)) hobo_i <- trim_data(hobo_i, start_date, end_date)

    check_n_rows(hobo_i, file_name = file_name, trimmed = trim)


    hobo_dat[[i]] <- hobo_i
  } # end loop over files

  hobo_out <- hobo_dat %>%
    map_df(rbind)

  # Return compiled data ----------------------------------------------------
  message("hobo_ph data compiled")

  tibble(hobo_out)
}
