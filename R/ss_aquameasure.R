#' @title Import data from aquaMeasure sensors
#'
#' @details The aquaMeasure data must be saved in a folder named aquameasure in
#'   .csv format.
#'
#' @param path File path to the aquaMeasure folder.
#'
#' @param file_name Name of the file to import, including file extension.
#'
#' @return Returns a data.frame of aquaMeasure data, with the same columns as in
#'   the original file.
#'
#' @author Danielle Dempsey
#'
#' @importFrom data.table fread
#' @importFrom dplyr %>% arrange as_tibble filter mutate n select
#' @importFrom janitor convert_to_datetime
#' @importFrom lubridate parse_date_time
#' @importFrom readxl read_excel
#' @importFrom stringr str_glue
#'
#' @export

ss_read_aquameasure_data <- function(path, file_name) {

  ext <- extract_file_extension(file_name)

  if(ext != "csv") stop(paste0("Cannot read file with extension: ", ext))

  # finish path
  path <- file.path(str_glue("{path}/{file_name}"))

  data.table::fread(path, header = TRUE, data.table = FALSE, na.strings = "")

}


#
##param force_POSIXct Logical argument indicating whether to convert the
#  timestamp column to POSIXct. Timestamp messages "undefined" and "xs after
# startup (time not set)" will be coerced to \code{NA} without a warning.
# # Export -----------------------------------------------------
#
# if (isTRUE(force_POSIXct)) {
#   dat <- dat %>%
#     mutate(
#       `Timestamp(UTC)` = suppressWarnings(
#         parse_date_time(`Timestamp(UTC)`, orders = c("Ymd HM", "Ymd HMS"))
#       )
#     )
# }
# dat




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
#' @importFrom dplyr %>% mutate select slice
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
                                     trim = TRUE){

  # make sure columns of serial.table are named correctly
  names(sn_table) <- c("sensor", "serial", "depth")
  sn_table <- sn_table %>%
    mutate(sensor_serial = glue("{sensor}-{serial}"))

  # extract the deployment start and end dates from deployment_dates
  dates <- extract_deployment_dates(deployment_dates)
  start_date <- dates$start
  end_date <- dates$end
  dates_char <- paste(format(start_date, "%Y-%b-%d"), "to", format(end_date, "%Y-%b-%d"))

  # initialize dateframe for storing the output
  am_dat <- list(NULL)

  # name of aquameasure folder (case-insensitive)
  folder <- list.files(path) %>%
    str_extract(regex("aquameasure", ignore_case = T)) %>%
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
    stop(glue("There are {length(dat_files)} csv files in {path};
                 expected {nrow(sn_table)} files"))
  }

  excel_files <- list.files(path, all.files = FALSE, pattern = "*xlsx|xls")

  if (isTRUE(verbose) && length(excel_files) > 0) {
    warning(glue("Can't compile excel files.
    {length(excel_files)} excel files found in hobo folder.
    \nHINT: Please re-export in csv format."))
  }

  # Import data -------------------------------------------------------------

  # loop over each aM file
  for(i in seq_along(dat_files)) {

    # check whether file is .csv or .xlsx
    file_name <- dat_files[i]

    am_i <- ss_read_aquameasure_data(path, file_name)

    am_colnames <- colnames(am_i)

    # Extract metadata --------------------------------------------------------

    # serial number
    sn_i <- am_i %>%
      slice(1) %>%
      select(Sensor) %>%
      separate(Sensor, into = c("sensor", "serial number"), sep = "-")
    sn_i <- sn_i$`serial number`

    # if the serial number doesn't match any of the entries in sn_table
    if(!(sn_i %in% sn_table$serial)){

      stop(glue("Serial number {sn_i} does not match any serial numbers in sn_table"))

    }

    vars <- extract_aquameasure_vars(am_colnames)

    # use serial number to identify the depth from sn_table
    sensor_info_i <- dplyr::filter(sn_table, serial == sn_i)


    am2 <- am_i %>%
      select(
        contains("stamp"),
        `Record Type`,
        contains("Dissolved Oxygen"),
        contains("Temperature"),
        contains("Salinity")
      ) %>%
      filter(`Record Type` %in% c("Dissolved Oxygen", "Temperature", "Salinity")) %>%
      pivot_wider(
        id_cols = c("Timestamp(UTC)"),
        names_from = "Record Type", values_from = all_of(vars)
      ) %>%
      mutate(
        deployment_range = paste(
          format(start.date, "%Y-%b-%d"), "to", format(end.date, "%Y-%b-%d")
        ),
        sensor = sensor_info_i$sensor_serial,
        depth = sensor_info_i$depth
      ) %>%
      select(
        deployment_range,
        timestamp_utc = contains("stamp"),
        sensor,
        depth,
        dissolved_oxygen_percent_saturation = contains("Dissolved Oxygen_Dissolved Oxygen"),
        temperature_degree_C = contains("Temperature_Temperature"),
        salinity_ppt = contains("Salinity_Salinity")
      )





    # extract date column header (includes UTC offset)
    # use pattern match for "stamp" to look for column named "timestamp"
    date_ref.i <- names(am_i)[grep("stamp", names(am_i))]

    # format deployment date range for metadata
    deployment_ref <-

    # Clean and format data ---------------------------------------------------
    if("Temperature" %in% colnames(dat.i) & "Temp(Water)" %in% colnames(dat.i)){
      warning("There is a column named Temperature and a column named Temp(Water) in sensor ",
              sensor.i)
    }

    # Re-name the "Temp(Water)" column to "Temperature"
    if(!("Temperature" %in% colnames(dat.i)) & "Temp(Water)" %in% colnames(dat.i)){
      dat.i <- dat.i %>% rename(Temperature = `Temp(Water)`)
    }

    ## check colnames of dat.i for "Temperature", "Dissolved Oxygen", and "Salinity"
    temp <- ifelse("Temperature" %in% colnames(dat.i), "Temperature", NA)
    DO <- ifelse("Dissolved Oxygen" %in% colnames(dat.i), "Dissolved Oxygen", NA)
    sal <- ifelse("Salinity" %in% colnames(dat.i), "Salinity", NA)

    # create vector of the variables in this file by removing NA
    vars.to.select <- c(temp, DO, sal)
    vars.to.select <- vars.to.select[which(!is.na(vars.to.select))]


    print(paste("found", vars.to.select, "in file", file.i), sep = " ")


    # filter out DATES that sensor was not set up
    # "undefined" or "(time not set)"
    dat.i <- dat.i %>%
      select(TIMESTAMP = `Timestamp(UTC)`, `Record Type`, all_of(vars.to.select)) %>%
      mutate(DATE_VALUES = str_detect(TIMESTAMP, "(time not set)")) %>%
      filter(TIMESTAMP != "undefined", DATE_VALUES == FALSE) %>%
      select(-DATE_VALUES) %>%
      convert_timestamp_to_datetime()  # convert the timestamp to a POSIXct object

    # trim to the dates in deployment.range
    # added four hours to end.date to account for AST
    # (e.g., in case the sensor was retrieved after 20:00 AST, which is 00:00 UTC **The next day**)
    if(trim == TRUE) {
      dat.i <- dat.i %>%
        filter(TIMESTAMP >= start.date, TIMESTAMP <= (end.date + hours(4)))
    }

    for(j in seq_along(vars.to.select)){

      var.j <- vars.to.select[j]

      aM.j <- dat.i %>%
        select(TIMESTAMP, `Record Type`, all_of(var.j)) %>%
        rename(PLACEHOLDER = 3) %>%
        filter(`Record Type` == var.j)


      if(nrow(aM.j) > 0) {

        aM.j <- aM.j %>%
          mutate(INDEX = as.character(c(1:n()))) %>%
          mutate(PLACEHOLDER = na_if(PLACEHOLDER, "ERR"),
                 PLACEHOLDER = round(as.numeric(PLACEHOLDER), digits = 3))

        if(var.j == "Dissolved Oxygen") {
          aM.j <- aM.j %>%
            mutate(PLACEHOLDER = if_else(PLACEHOLDER < 0, NA_real_, PLACEHOLDER))
        }

        NA.j <- sum(is.na(aM.j$PLACEHOLDER))

        if(NA.j > 1000) warning(paste(NA.j, "values in", file.i, "for variable", var.j, "are NA"))

        aM.j <- aM.j %>%
          transmute(INDEX,
                    TIMESTAMP = as.character(TIMESTAMP),
                    PLACEHOLDER = as.character(PLACEHOLDER)) %>%
          add_metadata(row1 = deployment_ref,
                       row2 = sensor.i,
                       row3 = paste(var.j, depth.i, sep = "-"),
                       row4 = c(date_ref.i, var.j))

        # merge data on the INDEX row
        aM_dat <- full_join(aM_dat, aM.j, by = "INDEX")
      }  else message(paste("No", var.j, "observations found for", sensor.i))


    } # end loop over variables


  } # end loop over files



    print("aquaMeasure data compiled")

    aM_dat


}



