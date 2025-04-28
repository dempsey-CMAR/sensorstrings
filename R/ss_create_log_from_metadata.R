#' @title Writes a deployment log from the metadata tracking sheet
#'
#' @details Imports the NSDFA tracking sheet, filters for the station and date
#'   of interest, re-formats into the deployment log format, and exports to the
#'   Log folder.
#'
#' @param path_metadata Path to the metadata tracking sheet (including the file
#'   name and extension). This must be a .xlsx file.
#'
#' @param sheet Name of the tab with the deployment information.
#'
#' @param path_export Path to the station deployment folder.
#'
#' @param station Station name.
#'
#' @param deployment_date Date of deployment as a character string in the format
#'   "YYYY-mm-dd"
#'
#' @param to_title Logical argument indicating whether to convert the station
#'   name to title case. Default is \code{TRUE}. Set to \code{FALSE} for
#'   stations with capital letters within a word, e.g., McNabs Island.
#'
#' @return Returns deployment log in .csv format.
#'
#' @importFrom readxl read_excel
#' @importFrom lubridate as_date
#' @importFrom stringr str_to_title
#' @importFrom dplyr %>% if_else filter mutate transmute
#' @importFrom utils write.csv
#'
#' @export

ss_create_log_from_metadata <- function(
    path_metadata = NULL,
    sheet = "tracker",
    path_export,
    station,
    deployment_date,
    to_title = TRUE
){

  if(is.null(path_metadata)) {
    path_metadata <- file.path(
      "R:/tracking_sheets/metadata_tracking/water_quality_deployment_tracking.xlsx"
    )
  }

  dat_raw <- read_excel(
    path_metadata,
    sheet = sheet,
    na = ""
  )

  if(isTRUE(to_title)) {
    station_title <- str_to_title(station)
  } else station_title <- station

  if(!(station_title %in% dat_raw$station)) {
    stop(paste0(station_title, " not found in metatdata tracking sheet"))
  }

  dat <- dat_raw %>%
    mutate(
      deployment_latitude = if_else(
        is.na(deployment_latitude),
        ss_coords_from_ddm_to_dd(deployment_latitude_n_ddm),
        deployment_latitude
      ),
      deployment_longitude = if_else(
        is.na(deployment_longitude),
        -ss_coords_from_ddm_to_dd(deployment_longitude_w_ddm),
        deployment_longitude
      ),
      retrieval_latitude = if_else(
        is.na(retrieval_latitude),
        ss_coords_from_ddm_to_dd(retrieval_latitude_n_ddm),
        retrieval_latitude
      ),
      retrieval_longitude = if_else(
        is.na(retrieval_longitude),
        -ss_coords_from_ddm_to_dd(retrieval_longitude_w_ddm),
        retrieval_longitude
      )
    )

  # make log
  log <- dat %>%
    filter(
      station == !!station_title,
      deployment_date == !!as_date(deployment_date)
    ) %>%
    select(
      county, waterbody, station, lease,

      deployment_date, retrieval_date,

      deployment_latitude, deployment_longitude,
      retrieval_latitude, retrieval_longitude,
      sensor_type, sensor_serial_number, sensor_depth_m,
      string_configuration
    ) %>%
    dplyr::mutate(
      #Location_Description = str_to_title(Location_Description),
      sensor_serial_number = as.numeric(sensor_serial_number),
      sensor_depth_m = as.numeric(sensor_depth_m),
      deployment_date = format(as_date(deployment_date), "%Y-%m-%d"),
      retrieval_date = format(as_date(retrieval_date), "%Y-%m-%d"),
    )

  if (nrow(log) == 0) {
    warning(
      paste("No rows found in metatdata for << ", station, deployment_date, " >>")
    )
  }

  # Format & Export ---------------------------------------------------------
  file_name <- paste(
    station, format(as_date(deployment_date), "%Y-%m-%d"),
    "log.csv", sep = "_"
  )

  path_export <- file.path(paste0(path_export, "/log"))

  if (!dir.exists(path_export)) dir.create(path_export)

  write.csv(
    log,
    file = paste(path_export, file_name, sep = "/"),
    row.names = FALSE
  )

  message(file_name, " exported to ", path_export)
}


