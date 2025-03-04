#' @title Read in deployment log
#'
#' @details The log must be saved in a folder called Log in .csv, .xlsx or .xls
#'   format, and must include the following columns:
#'
#'   \code{Deployment_Waterbody}: waterbody where string was deployed
#'
#'   \code{Location_Description}: the station name
#'
#'   \code{Lease#}: If located on an aquaculture site, the lease number (NA
#'   otherwise)
#'
#'   \code{Deployment}: deployment date, in the order "Ymd"
#'
#'   \code{Retrieval}: retrieval date, in the order "Ymd"
#'
#'   \code{Logger_Latitude}: The latitude at which the string was deployed
#'
#'   \code{Logger_Longitude} The longitude at which the string was deployed
#'   (must be a negative value)
#'
#'   \code{Logger_Model} The type of sensor; see below for options
#'
#'   \code{Serial#} The sensor serial number
#'
#'   \code{Sensor_Depth}: Depth at which the sensor was deployed
#'
#'   All other columns will be ignored.
#'
#'   Entries in the \code{Logger_Model} column must include the string
#'   "aquameasure", "hobo", "tidbit", or "vr2ar" (not case sensitive).
#'
#'   The function will stop with an Error if there is more than one eligible
#'   file (csv, .xlsx or .xls) in the Log folder.
#'
#'   The function will stop with an Error if there if the
#'   \code{Logger_Longitude} is a positive value.
#'
#'   A Warning message is printed to the console when the function does not
#'   recognize a sensor in the log.
#'
#'   A message is printed to the console when hobo, aquameasure, or vemco
#'   sensors are not found in the log.
#'
#'   A message is printed to the console if there is more than one unique entry
#'   in \code{Deployment_Waterbody}, \code{Location_Description},
#'   \code{Deployment}, \code{Retrieval}, \code{Logger_Latitude}, or
#'   \code{Logger_Longitude}.
#'
#' @param path File path to the Log folder, or full path to the log file,
#'   include file name and extension.
#'
#' @param parse Logical argument indicating whether to parse log into a list
#'   used by ss_compile functions.
#'
#' @return Returns a a data frame of the deployment metadata. Option to parse
#'   the information into a list using \code{ss_parse_log()}.
#'
#' @importFrom data.table fread
#' @importFrom dplyr %>% contains filter mutate select
#' @importFrom glue glue
#' @importFrom readxl read_excel
#' @importFrom utils file_test
#'
#' @export

ss_read_log_simple <- function(path, parse = TRUE) {
  # Read in log -----------------------------------------------------------
  # extract the name of the log folder (e.g. Log, log, LOG)

  if (length(path) > 1) {
    stop("More than one file path detected")
  }

  # if path goes to the deployment folder, finish with log folder + file name + extenstion
  # for cmar folder structure
  if (isFALSE(utils::file_test("-f", path))) {

    folder <- list.files(path) %>%
      str_extract(regex("log", ignore_case = TRUE)) %>%
      na.omit()

    path <- glue("{path}/{folder}")

    dat_files <- list.files(path, all.files = FALSE, pattern = "*xlsx|*xls|*csv")

    # remove files that start with "~"
    if (any(substring(dat_files, 1, 1) == "~")) {
      dat_files <- dat_files[-which(substring(dat_files, 1, 1) == "~")]
    }
    if (length(dat_files) > 1) {
      stop("More than one file found in the log folder")
    }
    path <- paste(path, dat_files, sep = "/")
  }

  # file extension
  file_type <- extract_file_extension(path)

  if (file_type == "xls" | file_type == "xlsx") {
    log <- read_excel(path, na = c("", "n/a", "N/A"))
  }

  if (file_type == "csv") {
    log <- fread(
      path,
      data.table = FALSE,
      na.strings = c("", "n/a", "N/A")
    )
  }

  if(isTRUE(parse)) log <- ss_parse_log(log)

  log
}


#'Extract information from deployment log
#'
#'log must include the following columns (or the information will be recorded as
#'NA):
#'
#'\code{county}: county where string was deployed
#'
#'\code{waterbody}: waterbody where string was deployed
#'
#'\code{station}: the station name
#'
#'\code{lease}: the aquaculture lease number, if applicable (NA otherwise)
#'
#'\code{deployment_date}: deployment date, in the order "YYYY-MM-DD"
#'
#'\code{retrieval_date}: retrieval date, in the order "YYYY-MM-DD"
#'
#'\code{deployment_latitude}: The latitude at which the string was deployed
#'
#'\code{deployment_longitude} The longitude at which the string was deployed
#'(must be a negative value)
#'
#'\code{sensor_type} The type of sensor
#'
#'\code{sensor_serial_number} The sensor serial number
#'
#'\code{sensor_depth_m}: Depth at which the sensor was deployed, in m
#'
#'All other columns will be ignored.
#'
#'Entries in the \code{sensor_type} column must include the string
#'"aquameasure", "hobo", "tidbit", or "vr2ar" (not case sensitive).
#'
#'The function will stop with an Error if there is more than one eligible file
#'(csv, .xlsx or .xls) in the Log folder.
#'
#'A warning is printed if \code{deployment_longitude} is a positive value.
#'
#'A Warning message is printed when the function does not recognize a sensor in
#'the log.
#'
#'A message is printed to the console if there is more than one unique entry in
#'\code{waterbody}, \code{station}, \code{deployment_date},
#'\code{retrieval_date}, \code{deployment_latitude}, or
#'\code{deployment_longitude}.
#'
#'@param log Log data frame, e.g., as exported from \code{ss_read_log_simple()}.
#'
#'@param deployment_dates Logical argument indicating whether to parse and
#'  return the deployment and retrieval dates.
#'
#'@param area_info  Logical argument indicating whether to parse and return the
#'  location details.
#'
#'@param sn_table  Logical argument indicating whether to parse and return the
#'  serial numbers and sensor depth.
#'
#'@param config  Logical argument indicating whether to parse and return the
#'  string configuration.
#'
#'@return Returns a list with up to 4 elements. \code{deployment_dates} is a
#'  data frame with two columns: \code{start_date} (the date of deployment) and
#'  \code{end_date} (date of retrieval). \code{area_info} is a data frame with
#'  five columns: \code{county}, \code{waterbody}, \code{latitude},
#'  \code{longitude}, \code{station}, and \code{lease}. \code{sn_table} is a
#'  data frame with three columns: \code{log_sensor} (sensor name as recorded in
#'  the log), \code{sensor_serial_number}, and \code{depth} (sensor depth below
#'  the surface at low tide, from the Sensor_Depth column).
#'  \code{string_configuration} is a character string indicating how the sensor
#'  string was moored (i.e., whether the sensors float with the tide).
#'
#'@importFrom dplyr %>% contains filter mutate select
#'@importFrom glue glue
#'@importFrom lubridate ymd
#'@importFrom readxl read_excel
#'@importFrom stringr str_replace str_detect
#'@importFrom tidyr separate
#'@importFrom utils file_test
#'
#'@export

ss_parse_log <- function(
    log,
    deployment_dates = TRUE,
    area_info = TRUE,
    sn_table = TRUE,
    config = TRUE
) {

  cols <- tolower(colnames(log))

  # deployment dates ------------------------------------------------------------

  if(isTRUE(deployment_dates)) {
    # deployment dates

    if("deployment_date" %in% cols) {
      depl_start <- unique(log$deployment_date)
    } else {
      message("Column deployment_date not found in log. Deployment date will be recorded as NA.")
      depl_start <- NA
    }

    if("retrieval_date" %in% cols) {
      depl_end <- unique(log$retrieval_date)
    } else {
      message("Column retrieval_date not found in log. Retrieval date will be recorded as NA.")
      depl_end <- NA
    }

    # message if there is more than one Deployment or Retrieval date
    if (length(depl_start) > 1) {
      warning("Multiple Deployment dates in log")
    }

    if (length(depl_end) > 1) {
      warning("Multiple Retrieval dates in log")
    }

    # WARNING if the dates are not in the proper format
    if (is.na(suppressWarnings(lubridate::ymd(depl_start[1]))) & !is.na(depl_start[1])) {
      warning("Deployment date must be in the order year, month, day")
    }
    if (is.na(suppressWarnings(lubridate::ymd(depl_end[1]))) & !is.na(depl_end[1])) {
      warning("Retrieval date must be in the order year, month, day")
    }

    # deployment date info to export
    deployment_dates <- data.frame(
      start_date = lubridate::ymd(depl_start),
      end_date = lubridate::ymd(depl_end)
    )

    # stop with Error if deployment date is after retrieval date
    if (deployment_dates$start_date[1] > deployment_dates$end_date[1]) {
      stop("The deployment date is after the retrieval date")
    }
  } else deployment_dates <- NULL

  # area info ---------------------------------------------------------------

  if(isTRUE(area_info)) {

    if("county" %in% cols) {
      county <- unique(log$county)
    } else county <- NA

    if("waterbody" %in% cols) {
      wb <- unique(log$waterbody)
      if (length(wb) > 1) message("Multiple waterbodies in log")
    } else wb <- NA

    if("deployment_latitude" %in% cols) {
      lat <- unique(log$deployment_latitude)
      if (length(lat) > 1) message("Multiple latitudes recorded in log")
    } else lat <- NA

    if("deployment_longitude" %in% cols) {
      long <- unique(log$deployment_longitude)
      if (length(long) > 1) message("Multiple longitudes recorded in log")
      if (any(long > 0)) stop("Longitude must be a negative value")
    } else long <- NA

    if("station" %in% cols) {
      station <- as.character(unique(log$station))
      if (length(station) > 1) message("Multiple stations recorded in log")
    } else station <- NA

    if("lease" %in% cols) {
      lease <- unique(log$lease)
      if (length(lease) > 1) message("Multiple leases recorded in log")
    } else lease <- NA

    area_info <- data.frame(
      county = county,
      waterbody = wb,
      latitude = lat,
      longitude = long,
      station = station,
      lease = lease
    )

  } else area_info <- NULL

  # depth -------------------------------------------------------------------

  if(isTRUE(sn_table)) {
    depth <- log %>%
      select(sensor_type, sensor_serial_number, sensor_depth_m) %>%
      mutate(
        numeric_depth = suppressWarnings(as.numeric(sensor_depth_m))
      )

    if (any(is.na((depth$numeric_depth)))) {
      depth_char <- depth %>%
        filter(is.na(numeric_depth)) %>%
        select(-numeric_depth) %>%
        mutate(
          depth_print = paste(
            sensor_type, sensor_serial_number, sensor_depth_m)
        )

      warning(
        paste0("Depth can't be converted to numeric: ", depth_char$depth_print, "\n")
      )
    }

    # serial number table -----------------------------------------------------

    sn_table <- log %>%
      select(
        log_sensor = sensor_type,
        sensor_serial_number,
        depth = sensor_depth_m
      )

    sensors <- sn_table %>%
      mutate(
        log_sensor = str_replace(log_sensor, "_", " "),
        log_sensor = tolower(log_sensor),
        detect_hobo = str_detect(log_sensor, "hobo"),
        detect_tidbit = str_detect(log_sensor, "tidbit"),
        detect_am = str_detect(log_sensor, "aquameasure"),
        detect_vemco = str_detect(log_sensor, "vr2ar")
      )

    # warning if there are any sensors in the log that are NOT recognized by package
    n_sensors <- sensors %>%
      select(contains("detect")) %>%
      apply(1, sum)

    if (any(n_sensors == 0)) {
      extra_sensor <- sensors[which(n_sensors == 0), "log_sensor"]

      warning(
        glue("{extra_sensor} found in the sensor_type column of the log.
            This sensor is not recognized by the sensorstrings package")
      )
    }
  } else sn_table <- NULL

  # configuration ------------------------------------------------------------

  if(isTRUE(config)) {
    config_options <- c(
      "sub-surface buoy", "surface buoy", "attached to gear",
      "attached to fixed structure", "floating dock", "unknown", "calval", NA
    )

    if ("configuration" %in% cols) {
      config <- unique(log$configuration)
    } else if ("mooring_type" %in% cols) {
      config <- unique(log$mooring_type)
    } else config <- NA

    if (!(config %in% config_options)) {
      warning("<< ", config, " >> is not an accepted sensor string configuration")
    }

    if (is.na(config)) {
      warning("Configuration will be converted from NA to << unknown >>")
      config <- "unknown"
    }

    if (length(config) > 1) {
      stop("More than one configuration type entered in the Log.")
    }
  } else config <- NULL

# return list of deployment info -------------------------------------------
list(
  deployment_dates = deployment_dates,
  area_info = area_info,
  sn_table = sn_table,
  string_configuration = config
)
}
