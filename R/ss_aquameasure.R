#' @title Import data from aquaMeasure sensors
#'
#' @details The aquaMeasure data must be saved in csv format.
#'
#' @inheritParams ss_read_hobo_data
#'
#' @param path File path to the aquameasure folder.
#'
#' @return Returns a data.frame of aquaMeasure data, with the same columns as in
#'   the original file.
#'
#' @author Danielle Dempsey
#'
#' @importFrom assertthat assert_that has_extension
#' @importFrom data.table fread
#' @importFrom stringr str_glue
#'
#' @export

ss_read_aquameasure_data <- function(path, file_name) {

  assert_that(has_extension(file_name, "csv"))

  # finish path
  path <- file.path(str_glue("{path}/{file_name}"))

  data.table::fread(
    path,
    header = TRUE, data.table = FALSE, na.strings = "" # might need to add ERR to na.strings
  )
}


#' Compiles temperature, dissolved oxygen, and/or salinity data from aquaMeasure
#' sensors
#' @description Compiles and formats data from aquaMeasure sensors.
#' @details The raw aquaMeasure data must be saved in a folder named aquaMeasure
#'   in .csv or .xlsx format.
#'
#'   Rows with \code{undefined} and \code{... (time not set)} values in the
#'   \code{Timestamp(UTC)} column are filtered out.
#'
#'   Negative Dissolved Oxygen values are converted to \code{NA}.
#'
#'   "ERR" values are converted to \code{NA}.
#'
#'   A warning message is printed if there are more than 1000 \code{NA} values
#'   for a given variable.
#'
#'   All columns in are imported as characters to ensure the timestamp is parsed
#'   correctly. Timestamp must be saved in excel as a number or a character in
#'   the order "ymd IMS p", "Ymd IMS p", "Ymd HM", "Ymd HMS", "dmY HM", or "dmY
#'   HMS".
#'
#'   There still may be parsing errors because there are not entries in every
#'   column. This should not affect the data compilation. To check, save the
#'   spreadsheet with a new name new, delete the column causing the error
#'   (likely the "Text" column), re-run the function, and verify that there is
#'   no parsing error.
#'
#' @inheritParams ss_compile_hobo_data
#'
#' @return Returns a dataframe or exports a spreadsheet with the data compiled
#'   from each of the aquaMeasure sensors. Columns alternate between timestamp
#'   (UTC, in the format "Y-m-d H:M:S") and variable value (rounded to three
#'   decimal places). Metadata at the top of each column indicates the
#'   deployment and retrieval dates, the sensor serial number, the variable and
#'   depth of the sensor, and the timezone of the timestamp.
#'
#'   To include the metadata, all values were converted to class
#'   \code{character}. To manipulate the data, the values must be converted to
#'   the appropriate class (e.g., \code{POSIXct} for the timestamps and
#'   \code{numeric} for variable values). This can be done using the function
#'   \code{convert_to_tidydata()}.
#' @family compile
#' @author Danielle Dempsey
#'
#' @importFrom dplyr %>% if_else mutate select slice
#' @importFrom glue glue
#' @importFrom lubridate parse_date_time
#' @importFrom stringr str_detect
#' @importFrom tidyr separate pivot_wider
#' @importFrom tidyselect all_of
#'
#' @export

ss_compile_aquameasure_data <- function(path,
                                        sn_table,
                                        deployment_dates,
                                        trim = TRUE,
                                        verbose = TRUE) {
  # set up & check for errors
  setup <- set_up_compile(
    path = path,
    sn_table = sn_table,
    deployment_dates = deployment_dates,
    sensor_make = "aquameasure",
    verbose = verbose
  )

  path = setup$path

  sn_table <- setup$sn_table

  start_date <- setup$dates$start
  end_date <- setup$dates$end

  dat_files <- setup$dat_files

  # initialize list for storing the output
  am_dat <- list(NULL)

  # Import data -------------------------------------------------------------

  # loop over each aM file
  for (i in seq_along(dat_files)) {

    # check whether file is .csv or .xlsx
    file_name <- dat_files[i]

    am_i <- ss_read_aquameasure_data(path, file_name)

    am_colnames <- colnames(am_i)

    # sn and timezone checks --------------------------------------------------

    # serial number
    sn_i <- am_i %>%
      slice(1) %>%
      select(Sensor) %>%
      separate(Sensor, into = c("sensor", "serial number"), sep = "-")
    sn_i <- sn_i$`serial number`

    # if the serial number doesn't match any of the entries in sn_table
    if (!(sn_i %in% sn_table$serial)) {
      stop(glue("Serial number {sn_i} does not match any serial numbers in sn_table"))
    }

    # check timezone
    date_tz <- extract_aquameasure_tz(am_colnames)

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
    sensor_info_i <- dplyr::filter(sn_table, serial == sn_i)

    vars <- extract_aquameasure_vars(am_colnames)

    # extract sensor depth
    am_i <- am_i %>%
      select(
        timestamp_ = contains("stamp"),
        `Record Type`,
        contains("Dissolved Oxygen"),
        contains("Temperature"),
        contains("Salinity")
      ) %>%
      filter(`Record Type` %in% c("Dissolved Oxygen", "Temperature", "Salinity")) %>%
      tidyr::pivot_wider(
        id_cols = "timestamp_",
        names_from = "Record Type", values_from = all_of(vars)
      ) %>%
      filter(
        !str_detect(timestamp_, "after"),
        !str_detect(timestamp_, "undefined")
      ) %>%
      select(
        timestamp_,
        do_percent_saturation = contains("Dissolved Oxygen_Dissolved Oxygen"),
        temperature_degree_C = contains("Temperature_Temperature"),
        salinity_psu = contains("Salinity_Salinity")
      ) %>%
      convert_timestamp_to_datetime()

    if (trim == TRUE) am_i <- trim_data(am_i, start_date, end_date)

    if ("do_percent_concentration" %in% am_colnames) {
      am_i <- am_i %>%
        mutate(
          # do_percent_saturation = na_if(do_percent_saturation, "ERR"),
          do_percent_saturation = if_else(
            do_percent_saturation < 0, NA_real_, do_percent_saturation
          )
        )
    }

    am_i <- am_i %>%
      mutate(
        deployment_range = paste(
          format(start_date, "%Y-%b-%d"), "to", format(end_date, "%Y-%b-%d")
        ),
        sensor = as.character(sensor_info_i$sensor_serial),
        depth = sensor_info_i$depth
      ) %>%
      select(
        deployment_range,
        timestamp_,
        sensor,
        depth,
        dissolved_oxygen_percent_saturation = contains("percent_sat"),
        temperature_degree_C,
        salinity_psu = contains("salinity")
      )

    colnames(am_i)[which(str_detect(colnames(am_i), "timestamp"))] <- paste0("timestamp_", date_tz)

    am_dat[[i]] <- am_i
  } # end loop over files

  am_out <- am_dat %>%
    map_df(rbind)

  message("aquaMeasure data compiled")

  am_out
}
