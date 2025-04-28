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

ss_create_old_log_from_metadata <- function(
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
    transmute(
      Deployment_Waterbody = waterbody,
      Location_Description = station_title,
      `Lease#` = lease,
      Status = status,

      Deployment = deployment_date,
      Retrieval =  retrieval_date,
      Duration = difftime(retrieval_date, deployment_date),
      Logger_Latitude = deployment_latitude,
      Logger_Longitude = deployment_longitude,
      Logger_Model = sensor_type,
      `Serial#` = sensor_serial_number,
      Sensor_Depth = sensor_depth_m,

      Configuration = string_configuration,

      Sounding = sounding_m,
      Datum = NA,
      Mount_type = NA,

      #`Acoustic_Release?` = ifelse(Recv_Method == "Acoustic_Release", "Y", NA_character_),
      #`Surface_Buoy?` = ifelse(Recv_Method == "Surface Buoy", "Y", NA_character_),

      Deployment_Attendant = deployment_attendant,
      Retrieval_Attendant = retrieval_attendant,

      Comments = notes,

      # `Deployment Waypoint` = NA,
      #`Retrieval Waypoint` = NA,

      `Retrieval Latitude`	= retrieval_latitude,
      `Retrieval Longitude` =	retrieval_longitude,

      #`Sensor Voltage deployed` = Depl_Voltage,
      #`Sensor Voltage retrieved` = Recv_Voltage,

      #`Vessel sounder offset + transponder depth` = NA,
      #`verified measurement (below origin, first sensor under float)` = NA,
      #`tide correction` = NA,
      #`Rising or Falling` = NA,
      #`height of VR2AR base off bottom` = NA,

      #`time of deployment` = Depl_Time,
      #`photos taken?` = NA,

      #`Anchor type` = Anchor_Wgt,
      #`Float type` = NA,
      #`distance from top of float to origin (first sensor)` = NA
    ) %>%
    dplyr::mutate(
      #Location_Description = str_to_title(Location_Description),
      `Serial#` = as.numeric(`Serial#`),
      Sensor_Depth = as.numeric(Sensor_Depth),
      Sounding = as.numeric(Sounding),
      Deployment = format(as_date(Deployment), "%Y-%b-%d"),
      Retrieval = format(as_date(Retrieval), "%Y-%b-%d"),
    )

  if (nrow(log) == 0) {
    warning(
      paste("No rows found in metatdata for << ", station, deployment_date, " >>")
    )
  }

  # if(suppressWarnings(is.numeric(as.numeric(LOG$Sensor_Depth)))){
  #   LOG <- LOG %>% mutate(Sensor_Depth = as.numeric(Sensor_Depth))
  # }

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


