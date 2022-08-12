#' @title Import data from HOBO and TidbiT sensors
#'
#' @param path File path to the hobo file.
#'
#' @param file_name Name of the file to import, including file extension.
#'
#' @return Returns a tibble of HOBO data, with the same columns as in the
#'   original file.
#'
#' @author Danielle Dempsey
#'
#' @importFrom data.table fread
#' @importFrom stringr str_glue
#'
#' @export


ss_read_hobo_data <- function(path, file_name) {

  # finish path
  path <- file.path(str_glue("{path}/{file_name}"))

  # read in data
  # start with row that includes the "Date" header
  data.table::fread(path, skip = "Date", encoding =  "UTF-8")

}




#' @title Compile and format data from HOBO and TidbiT sensors
#'
#' @description Can handle temperature and dissolved oxygen data.
#'
#' Need to decide if the DO data will be corrected for salinity!
#'
#'   Only works for csv files - will ignore .xls and .xlsx files without an
#'   error.
#'
#'   All of the csv files in the Hobo folder will be compiled. The name of each
#'   file must be the serial number of the sensor,
#'
#'   The timestamp columns must be in the order "ymd IMS p", "Ymd IMS p", "Ymd
#'   HM", "Ymd HMS", "dmY HM", or "dmY HMS".
#'
#' @param path File path to the Hobo folder.
#'
#' @param sn_table UPDATE THIS A table with the serial number of each HOBO and TidBiT sensor
#'   on the string, in the form "HOBO-xxxxxxxx" or "TidbiT-xxxxxxxx" (first
#'   column) and corresponding depth at which it was deployed in the form "2m"
#'   (second column).
#'
#' @param deployment_dates A dataframe with two columns. The first column holds
#'   the deployment date (a Date object, POSIXct object, or character string in
#'   the order year, month, day),  and the second column holds the retrieval
#'   date (a Date object, POSIXct object, or character string in the order year,
#'   month, day).
#'
#' @param trim Logical value indicating whether to trim the data to the dates
#'   specified in /code{deployment_dates}. (Note: four hours are added to the
#'   retrieval date to account for AST, e.g., in case the sensor was retrieved
#'   after 20:00 AST, which is 00:00 UTC the next day.) Default is /code{trim =
#'   TRUE}.
#'
#' @param DO_correction Logical value. If /code{TRUE}, dissolved oxygen will be
#'   corrected for salinity (argument /code{Sal} must be specified).
#'
#'   If /code{FALSE}, no correction factor will be applied. If the sensor did
#'   not record dissolved oxygen data, use the default /code{correct.DO =
#'   FALSE}.
#'
#' @param Sal A single value of salinity (psu), used to calculate the salinity
#'   correction factor for dissolved oxygen.
#'
#' @return Returns a dataframe/tibble with the data compiled from each of the
#'   HOBO and TidbiT sensors.
#'
#' @family compile
#'
#' @author Danielle Dempsey
#'
#' @importFrom dplyr %>% contains filter mutate rename select
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
                                 trim = TRUE,
                                 DO_correction = FALSE,
                                 Sal = NULL) {

  names(sn_table) <- c("sensor", "serial", "depth")
  sn_table <- sn_table %>%
    mutate(sensor_serial = glue("{sensor}-{serial}"))

  # extract the deployment start and end dates from deployment_dates
  dates <- extract_deployment_dates(deployment_dates)
  start_date <- dates$start
  end_date <- dates$end
  dates_char <- paste(format(start_date, "%Y-%b-%d"), "to", format(end_date, "%Y-%b-%d"))

  # initialize list for storing the output
  hobo_dat <- list(NULL)

  # name of hobo folder (case-insensitive)
  folder <- list.files(path) %>%
    str_extract(regex("hobo", ignore_case = T)) %>%
    na.omit()

  # path to hobo files
  path <- glue("{path}/{folder}")

  # list files in the Hobo folder
  dat_files <- list.files(path, all.files = FALSE, pattern = "*csv")


  # check for surprises in dat_files -----------------------------------------------------

  if (length(dat_files) == 0) {

    stop(glue("Can't find csv files in {path}"))

  }

  if (length(dat_files) != nrow(sn_table)) {
    warning(glue("There are {length(dat_files)} csv files in {path};
                 expected {nrow(sn_table)} files"))
  }

  excel_files <- list.files(path, all.files = FALSE, pattern = "*xlsx|xls")

  if (length(excel_files) > 0) {
    warning(glue("Can't compile excel files.
    {length(excel_files)} excel files found in hobo folder.
    \nHINT: Please re-export in csv format."))
  }


  # loop over each HOBO file
  for (i in seq_along(dat_files)) {

    # Import Data -------------------------------------------------------------
    file_name <- dat_files[i]

    hobo_i <- ss_read_hobo_data(path, file_name)

    # combine these two functions
    hobo_units <- ss_extract_hobo_units(hobo_i)
    new_col_names <- ss_make_column_names(hobo_units)


    # sn and timezone checks --------------------------------------------------

    # is the file named correctly? Compare the file name to the serial number extracted from the file
    file_name <- str_remove(file_name, ".csv")
    sn_i <- extract_hobo_sn(colnames(hobo_i))

    if (file_name != sn_i) {
      warning(glue("The name of file {file_name} does not match the expected serial number ({sn_i})"))
    }

    # is this file in the sn_table?
    if (!(file_name %in% sn_table$serial)) {
      stop(glue("The name of file {file_name} does not match any serial numbers in sn_table"))
    }

    # is the time zone set to GMT+00:00?
    tz_i <- filter(hobo_units, str_detect(variable, pattern = "Date"))

    if (tz_i$units != "utc") {
      warning(glue("The timezone of file {file_name} is not UTC.\nTimezone: {tz_i$units}"))
    }


    # Select and add columns of interest ----------------------------------------------

    # reorder this so not double re-naming the data
    # put sensor info in after


    hobo_i <- hobo_i %>%
      select(contains("Date Time"), contains("DO conc"), contains("Temp")) %>%
      rename(timestamp_ = 1) %>%
      convert_timestamp_to_datetime()

    colnames(hobo_i) <- new_col_names$col_name

    # add other useful columns and re-order ------------------------------------------------

    # use serial number to identify the variable and depth (from sn_table)
    sensor_info_i <- dplyr::filter(sn_table, serial == sn_i)

    hobo_i2 <- hobo_i %>%
      mutate(
        deployment_range = paste(
          format(start_date, "%Y-%b-%d"), "to", format(end_date, "%Y-%b-%d")
        ),
        sensor = sensor_info_i$sensor_serial,
        depth = sensor_info_i$depth
      ) %>%
      select(
        deployment_range,
        timestamp_ = contains("timestamp"),
        sensor,
        depth,
        contains("dissolved_oxygen"),
        contains("temperature")
      )


    # if (!("dissolved_oxygen" %in% new_col_names$variable)) {
    #
    #
    #
    # }


    # # apply salinity correction factor to dissolved oxygen
    # if(isTRUE(correct.DO) & "Dissolved Oxygen" %in% names(hobo.i)) {
    #
    #   if(is.null(Sal)){
    #
    #     stop("Can't calculate salinity correction factor.
    #          /nHINT: Enter a value for the Sal argument or set correct.DO to FALSE")
    #   }
    #
    #   hobo.i <- hobo.i %>%
    #     DO_salinity_correction(Sal = Sal, method = method) %>%
    #     mutate(
    #       `Dissolved Oxygen` = as.numeric(`Dissolved Oxygen`),
    #       `Dissolved Oxygen` = `Dissolved Oxygen` * F_s
    #       ) %>%
    #     select(-F_s, -Salinity)
    #
    #     # apply_salinity_correction(Sal = Sal) %>%
    #     # dplyr::rename(`Dissolved Oxygen` = DO_corrected)
    # }

    # vars.to.select <- colnames(hobo.i)[-1]


    # Format data -------------------------------------------------------------

    # trim to the dates in deployment.range
    # added four hours to end.date to account for AST
    # (e.g., in case the sensor was retrieved after 20:00 AST, which is 00:00 UTC **The next day**)
    if (trim == TRUE) {
      hobo.i <- hobo.i %>%
        filter(timestamp_ >= start_date, timestamp_ <= (end_date + hours(4)))
    }

    colnames(hobo_i)[2] <- new_col_names$col_name[1]

    hobo_dat[[i]] <- hobo_i
  } # end loop over files



  hobo_out <- hobo_dat %>%
    map_df(rbind)



  # Return compiled data ----------------------------------------------------

  print("HOBO data compiled")

  HOBO_dat
}
