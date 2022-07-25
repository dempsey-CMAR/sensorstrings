#' @title Compiles temperature data from HOBO and TidbiT sensors
#'
#' @description Compiles and formats data from HOBO and TidbiT sensors.
#'
#' @param path File path to the Hobo folder. All of the excel files in the
#'   Hobo folder will be compiled. The name of each file must be the serial
#'   number of the sensor, and the excel files must be in either .csv or .xlsx
#'   format. The timestamp columns must be in the order "ymd IMS p", "Ymd IMS
#'   p", "Ymd HM", "Ymd HMS", "dmY HM", or "dmY HMS".
#'
#' @param sn_table A table with the serial number of each HOBO and
#'   TidBiT sensor on the string, in the form "HOBO-xxxxxxxx" or
#'   "TidbiT-xxxxxxxx" (first column) and corresponding depth at which it was
#'   deployed in the form "2m" (second column).
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
#' @return Returns a dataframe/tibble with the data compiled
#'   from each of the HOBO and TidbiT sensors.
#'
#' @family compile
#'
#' @author Danielle Dempsey
#'
#'
#'
#' @export



ss_compile_hobo_data <- function(path,
                                 sn_table,
                                 deployment_dates,
                                 trim = TRUE,
                                 DO_correction = FALSE,
                                 Sal = NULL,
                                 method = "garcia-gordon") {
  names(sn_table) <- c("SENSOR", "DEPTH")
  sn_table <- sn_table %>%
    separate(col = SENSOR, into = c(NA, "SERIAL"), remove = FALSE)

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

  if (length(dat_files) != nrow(sn_table)) {
    warning(glue("There are {length(dat_files)} csv files in {path};
                 expected {nrow(sn_table)} files"))
  }


  # loop over each HOBO file
  for (i in seq_along(dat_files)) {

    # Import Data -------------------------------------------------------------
    file_name <- dat_files[i]

    hobo_i <- ss_read_hobo_data(path, file_name)


    # sn and timezone checks --------------------------------------------------

    # is the file named correctly? Compare the file name to the serial number exracted from the file
    file_name <- str_remove(file_name, ".csv")
    sn_i <- extract_hobo_sn(colnames(hobo_i))

    if (file_name != sn_i) {
      warning(glue("The name of file {file_name} does not match the expected serial number ({sn_i})"))
    }

    # is this file in the sn_table?
    if (!(file_name %in% sn_table$SERIAL)) {
      stop(glue("The name of file {file_name} does not match any serial numbers in sn_table"))
    }

    # is the time zone set to GMT+00:00?
    tz_i <- extract_hobo_tz(colnames(hobo_i))

    if (tz_i != "GMT+00:00") {
      warning(glue("The timezone of file {file_name} is not UTC.\nTimezone: {tz_i}"))
    }







    # use serial number to identify the variable and depth (from serial.table)
    depth_i <- sn_table %>%
      dplyr::filter(SERIAL == sn_i) %>%
      select(DEPTH)
    depth_i <- depth_i$DEPTH

    # sensor type and serial number
    # sensor_i <- paste(serial.table.HOBO$SENSOR[i], serial.table.HOBO$SERIAL[i], sep = "-")
    sensor_i <- glue("HOBO-{SN_i}")




    # Select columns of interest ----------------------------------------------

    # select columns of interest
    hobo2 <- hobo_i %>%
      select(
        TIMESTAMP = contains("Date"),
        Temperature = contains("Temp"),
        `Dissolved Oxygen` = contains("DO conc, mg/L")
      ) %>%
      mutate(
        DEPTH = depth_i,
        SENSOR = sensor_i,
        DEPLOYMENT_PERIOD = dates_char
      )

    # %>%
    #   select(
    #     contains("Date"),
    #     contains("Temp"),
    #     contains("DO")
    #   )
    #




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

    vars.to.select <- colnames(hobo.i)[-1]


    # Format data -------------------------------------------------------------

    # trim to the dates in deployment.range
    # added four hours to end.date to account for AST
    # (e.g., in case the sensor was retrieved after 20:00 AST, which is 00:00 UTC **The next day**)
    if (trim == TRUE) {
      hobo.i <- hobo.i %>%
        filter(TIMESTAMP >= start_date, TIMESTAMP <= (end_date + hours(4)))
    }

    HOBO_dat[[i]] <- hobo_i
  } # end loop over files



  HOBO_dat2 <- HOBO_dat %>%
    map_df(rbind)


  if ("Dissolved Oxgen" %in% colnames(hobo2)) {
    hobo3 <- hobo2 %>%
      pivot_longer(
        c("Temperature", "Dissolved Oxygen"),
        names_to = "VARIABLE", values_to = "VALUE"
      )
  } else {
    hobo3 <- hobo2 %>%
      pivot_longer(Temperature, names_to = "VARIABLE", values_to = "VALUE")
  }



  # Return compiled data ----------------------------------------------------

  print("HOBO data compiled")

  HOBO_dat
}
