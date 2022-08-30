#' Import data from Hobo and TidbiT sensors
#'
#' @details The Hobo data must be saved in csv format.
#'
#' @param path File path to the hobo file.
#'
#' @param file_name Name of the file to import, including file extension.
#'
#' @return Returns a data frame of Hobo data, with the same columns as in the
#'   original file.
#'
#' @author Danielle Dempsey
#'
#' @importFrom assertthat assert_that has_extension
#' @importFrom data.table fread
#' @importFrom stringr str_glue
#'
#' @export

ss_read_hobo_data <- function(path, file_name) {
  assert_that(has_extension(file_name, "csv"))

  # finish path
  path <- file.path(str_glue("{path}/{file_name}"))

  # read in data
  # start with row that includes the "Date" header
  # use UTF-8 coding for degree symbol
  # return as data.frame (not data.table)
  data.table::fread(path,
                    skip = "Date",
                    encoding = "UTF-8", data.table = FALSE)

}


#' @title Compile and format data from Hobo and TidbiT sensors
#'
#' @description Can handle temperature and dissolved oxygen data.
#'
#'   Need to decide if the DO data will be corrected for salinity!
#'
#' @details The exported hobo data must be saved in a folder named Hobo in csv
#'   format. Folder name is not case-sensitive.
#'
#'   All of the csv files in the Hobo folder will be compiled. The name of each
#'   file must be the serial number of the sensor,
#'
#'   The timestamp columns must be in the order "ymd IMS p", "Ymd IMS p", "Ymd
#'   HM", "Ymd HMS", "dmY HM", or "dmY HMS".
#'
#' @param path File path to the Hobo folder.
#'
#' @param sn_table A data frame with three columns: \code{sensor}, code{serial},
#'   \code{depth}, as returned by \code{ss_read_log()}.
#'
#' @param deployment_dates A dataframe with two columns. The first column holds
#'   the deployment date (a Date object, POSIXct object, or character string in
#'   the order year, month, day), and the second column holds the retrieval date
#'   (a Date object, POSIXct object, or character string in the order year,
#'   month, day).
#'
#' @param trim Logical value indicating whether to trim the data to the dates
#'   specified in \code{deployment_dates}. (Note: four hours are added to the
#'   retrieval date to account for AST, e.g., in case the sensor was retrieved
#'   after 20:00 AST, which is 00:00 UTC the next day.) Default is \code{trim =
#'   TRUE}.
#'
#' @return Returns a tibble with the data compiled from each of the HOBO and
#'   TidbiT sensors in path/Hobo.
#'
#' @family compile
#'
#' @author Danielle Dempsey
#'
#' @importFrom dplyr %>% contains filter mutate rename select tibble
#' @importFrom glue glue
#' @importFrom lubridate hours
#' @importFrom purrr map_df
#' @importFrom stats na.omit
#' @importFrom stringr regex str_extract str_remove
#'
#' @export

ss_compile_hobo_data <- function(path,
                                 sn_table,
                                 deployment_dates,
                                 trim = TRUE) {
  # set up & check for errors
  setup <- set_up_compile(
    path = path,
    sn_table = sn_table,
    deployment_dates = deployment_dates,
    sensor_make = "hobo"
  )

  path = setup$path

  sn_table <- setup$sn_table

  start_date <- setup$dates$start
  end_date <- setup$dates$end

  dat_files <- setup$dat_files

  # initialize list for storing the output
  hobo_dat <- list(NULL)

  # loop over each HOBO file
  for (i in seq_along(dat_files)) {

    # Import Data -------------------------------------------------------------
    file_name <- dat_files[i]

    hobo_i <- ss_read_hobo_data(path, file_name)

    # combine these two functions
    hobo_units <- extract_hobo_units(hobo_i)
    new_col_names <- make_column_names(hobo_units)

    # sn and timezone checks --------------------------------------------------

    file_name <- str_remove(file_name, ".csv")
    sn_i <- extract_hobo_sn(colnames(hobo_i))

    tz_i <- filter(hobo_units, str_detect(variable, pattern = "Date"))

    if (file_name != sn_i) {
      stop(glue("The name of file {file_name} does not match the expected serial number ({sn_i})"))
    }

    # is this file in the sn_table
    if (!(file_name %in% sn_table$serial)) {
      stop(glue("The name of file {file_name} does not match any serial numbers in sn_table"))
    }

    # is the time zone set to GMT+00:00?
    if (tz_i$units != "utc") {
      warning(glue("The timezone of file {file_name} is not UTC.\nTimezone: {tz_i$units}"))
    }

    # Select and add columns of interest ----------------------------------------------
    hobo_i <- hobo_i %>%
      select(contains("Date Time"), contains("DO conc"), contains("Temp")) %>%
      rename(timestamp_ = 1) %>%
      convert_timestamp_to_datetime()

    colnames(hobo_i) <- new_col_names$col_name

    # add other useful columns and re-order ------------------------------------------------

    # use serial number to identify the variable and depth (from sn_table)
    sensor_info_i <- dplyr::filter(sn_table, serial == sn_i)

    hobo_i <- hobo_i %>%
      mutate(
        deployment_range = paste(
          format(start_date, "%Y-%b-%d"), "to", format(end_date, "%Y-%b-%d")
        ),
        sensor = as.character(sensor_info_i$sensor_serial),
        sensor_depth_at_low_tide_m = sensor_info_i$depth
      ) %>%
      select(
        deployment_range,
        contains("timestamp"),
        sensor,
        sensor_depth_at_low_tide_m,
        contains("dissolved_oxygen"),
        contains("temperature")
      )

    check_n_rows(hobo_i, file_name = file_name, trimmed = FALSE)

    # trim to the dates in deployment_dates
    if (isTRUE(trim)) hobo_i <- trim_data(hobo_i, start_date, end_date)

    check_n_rows(hobo_i, file_name = file_name, trimmed = trim)


    hobo_dat[[i]] <- hobo_i
  } # end loop over files

  hobo_out <- hobo_dat %>%
    map_df(rbind)

  # Return compiled data ----------------------------------------------------
  message("HOBO data compiled")

  tibble(hobo_out)
}
