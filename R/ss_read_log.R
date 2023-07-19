#' @title Extract information from deployment log
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
#'   \code{Deployment}: The deployment date, in the order "Ymd"
#'
#'   \code{Retrieval}: The retrieval date, in the order "Ymd"
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
#'   A message will be printed to the console if there is more than one unique
#'   entry in \code{Deployment_Waterbody}, \code{Location_Description},
#'   \code{Deployment}, \code{Retrieval}, \code{Logger_Latitude}, or
#'   \code{Logger_Longitude}.
#'
#' @param path File path to the Log folder.
#'
#' @param path_config File path (including name and extension) of the water
#'   quality configuration table. Default is
#'   water_quality_configuration_table.xlsx in the tracking_sheets folder. Data
#'   must be stored in an xlsx file in a tab named "deployments". Columns must
#'   be Waterbody, Station_Name, and Depl_Date. One and only one row must match
#'   the waterbody, station, and deployment date recorded in the Log. Options
#'   for the configuration column are sub-surface buoy, surface buoy, attached
#'   to gear, attached to fixed structure, floating dock, and unknown. Use of
#'   the "unknown" entry is discouraged. HINT: for optimal speed, make sure the
#'   table is NOT filtered in the excel file.
#'
#'   The \code{path_config} argument is ignored for new deployment logs that
#'   have an appropriate entry in the Configuration column.
#'
#' @return Returns a list with 4 elements. \code{deployment_dates} is a data
#'   frame with two columns: \code{start_date} (the date of deployment) and
#'   \code{end_date} (date of retrieval). \code{area_info} is a data frame with
#'   five columns:\code{county}, \code{waterbody}, \code{latitude},
#'   \code{longitude}, \code{station}, and \code{lease}. \code{sn_table} is a
#'   data frame with three columns: \code{log_sensor} (sensor name as recorded
#'   in the log), \code{sensor_serial_number}, and \code{depth} (sensor depth
#'   below the surface at low tide, from the Sensor_Depth column).
#'   \code{string_configuration} is a character string indicating how the sensor
#'   string was moored (i.e., whether the sensors float with the tide).
#'
#' @family compile
#' @author Danielle Dempsey
#'
#' @importFrom data.table fread
#' @importFrom dplyr %>% contains filter mutate select
#' @importFrom glue glue
#' @importFrom googlesheets4 gs4_deauth read_sheet
#' @importFrom lubridate ymd
#' @importFrom readxl read_excel
#' @importFrom stringr str_replace str_detect
#' @importFrom tidyr separate
#'
#' @export

ss_read_log <- function(path, path_config = NULL) {
  # Read in log -----------------------------------------------------------
  # extract the name of the log folder (e.g. Log, log, LOG)
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
    stop("More than one file found in the Log folder")
  }

  # file extension
  file_type <- extract_file_extension(dat_files)

  if (file_type == "xls" | file_type == "xlsx") {
    log <- read_excel(paste(path, dat_files, sep = "/"), na = c("", "n/a", "N/A"))
  }

  if (file_type == "csv") {
    log <- fread(
      paste(path, dat_files, sep = "/"),
      data.table = FALSE,
      na.strings = c("", "n/a", "N/A")
    )
  }


  # deployment dates ------------------------------------------------------------

  # deployment dates
  depl_start <- unique(log$Deployment)
  depl_end <- unique(log$Retrieval)

  # message if there is more than one Deployment or Retrieval date
  if (length(depl_start) > 1) {
    warning("Multiple Deployment dates in log")
  }

  if (length(depl_end) > 1) {
    warning("Multiple Retrieval dates in log")
  }

  # Stop with ERROR if the dates are not in the proper format
  if (is.na(suppressWarnings(lubridate::ymd(depl_start[1])))) {
    stop("Deployment date must be in the order year, month, day")
  }
  if (is.na(suppressWarnings(lubridate::ymd(depl_end[1])))) {
    stop("Retrieval date must be in the order year, month, day")
  }

  # deployment info to export
  deployment_dates <- data.frame(
    start_date = lubridate::ymd(depl_start),
    end_date = lubridate::ymd(depl_end)
  )

  # area info ---------------------------------------------------------------

  wb <- unique(log$Deployment_Waterbody)
  lat <- unique(log$Logger_Latitude)
  long <- unique(log$Logger_Longitude)
  station <- as.character(unique(log$Location_Description))
  lease <- unique(log$`Lease#`)

  if (length(wb) > 1) message("Multiple waterbodies in log")
  if (length(lat) > 1) message("Multiple latitudes recorded in log")
  if (length(long) > 1) message("Multiple longitudes recorded in log")
  if (length(station) > 1) message("Multiple location descriptions recorded in log")
  if (length(lease) > 1) message("Multiple leases recorded in log")

  if (any(long > 0)) stop("Longitude must be a negative value")

  # link to the "STRING TRACKING" google sheet -
  googlesheets4::gs4_deauth()

  link <- "http://docs.google.com/spreadsheets/d/1a3QvJsvwr4dd64g3jxgewRtfutIpsKjT2yrMEAoxA3I/edit#gid=828367890"

  # read in the "Area Info" tab of the STRING TRACKING sheet
  area_tracking <- googlesheets4::read_sheet(link, sheet = "Area Info")

  # look up the Station name in the Area Info tab and return the county
  county <- area_tracking[
    which(area_tracking$station == station & area_tracking$waterbody == wb),
    "county"
  ]$county

  if (length(county) > 1) {
    stop("There is more than one station named << ", station, " >>
         with waterbody <<", wb, " >> in the Area Info tab")
  }
  if (length(county) < 1) {
    stop("There is no station named << ", station, " >>
         with waterbody << ", wb, " >> in the Area Info tab")
  }

  area_info <- data.frame(
    county = county,
    waterbody = log$Deployment_Waterbody[1],
    latitude = log$Logger_Latitude[1],
    longitude = log$Logger_Longitude[1],
    station = log$Location_Description[1],
    lease = log$`Lease#`[1]
  )


  # depth -------------------------------------------------------------------

  depth <- log %>%
    select(Logger_Model, `Serial#`, Sensor_Depth) %>%
    mutate(numeric_depth = suppressWarnings(as.numeric(Sensor_Depth)))

  if (any(is.na((depth$numeric_depth)))) {
    depth_char <- depth %>%
      filter(is.na(numeric_depth)) %>%
      select(-numeric_depth) %>%
      mutate(depth_print = paste(Logger_Model, `Serial#`, Sensor_Depth))

    warning(
      paste0("Depth can't be converted to numeric: ", depth_char$depth_print, "\n")
    )
  }

  # serial number table -----------------------------------------------------

  sn_table <- log %>%
    select(
      log_sensor = Logger_Model,
      sensor_serial_number = `Serial#`,
      # do not conver this to numeric - will cause error if there are quali depths
      depth = Sensor_Depth
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
      glue("{extra_sensor} found in the Logger_Model column of the log.
            This sensor is not recognized by the sensorstrings package")
    )
  }

  # message if any expected sensors not found
  if (nrow(filter(sensors, detect_hobo == TRUE)) == 0) {
    message("No Hobo sensors found in log")
  }

  if (nrow(filter(sensors, detect_am == TRUE)) == 0) {
    message("No aquaMeasure sensors found in log")
  }

  if (nrow(filter(sensors, detect_vemco == TRUE)) == 0) {
    message("No VR2AR sensors found in log")
  }

  # configuration ------------------------------------------------------------

  config_options <- c(
    "sub-surface buoy", "surface buoy", "attached to gear",
    "attached to fixed structure", "floating dock", "unknown"
  )

  if ("configuration" %in% tolower(colnames(log))) {
    config <- unique(log$configuration)

    if (is.na(config)) {
      stop("Configuration is recorded as NA in the Log.")
    }

    if (length(config) > 1) {
      stop("More than one configuration type entered in the Log.")
    }
  } else {
    if (is.null(path_config)) {
      path_config <- file.path(
        "Y:/coastal_monitoring_program/tracking_sheets/water_quality_configuration_table.xlsx"
      )
    }

    config <- read_excel(path_config, sheet = "deployments") %>%
      mutate(Deployment = as_date(Depl_Date)) %>%
      filter(
        Station_Name == area_info$station,
        Waterbody == area_info$waterbody,
        Depl_Date == deployment_dates$start_date[1]
      )

    if (nrow(config) == 0) {
      stop("Deployment not found in Configuration table.
          \nHINT: check station, waterbody, and deployment date in log and Configuration table")
    }

    config <- config$Configuration


    if (is.na(config)) {
      stop("Configuration is recorded as NA in the Configuration table")
    }

    if (length(config) > 1) {
      stop(
        "More than one Configuration for << ", area_info$station, " >> deployed on << ",
        deployment_dates$start_date, " >> recorded in the Configuration table"
      )
    }
  }


  if (!(config %in% config_options)) {
    warning("<< ", config, " >> is not an accepted sensor string configuration")
  }


  # return list of deployment info -------------------------------------------
  list(
    deployment_dates = deployment_dates,
    area_info = area_info,
    sn_table = sn_table,
    string_configuration = config
  )
}
