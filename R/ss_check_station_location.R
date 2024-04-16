#' Check station location is within buffer of official coordinates
#'
#' @param log_coords Data frame with three columns: \code{station},
#'   \code{latitude} and \code{longitude}, i.e., with the station coordinates as
#'   recorded in the deployment log.
#' @param log_crs The crs of the coordinates in \code{log_coords}.
#' @param station_coords Data frame with two columns: \code{latitude} and
#'   \code{longitude}, the "official" station coordinates. If \code{NULL}, the
#'   station coordinates will be looked up in the AREA INFO tracking sheet.
#' @param station_radius Acceptable radius around the official station
#'   coordinates (in metres). Stations outside of this radius should be renamed.
#'   Default is 500 m.
#'
#' @return Logical value. Returns \code{TRUE} if the log coordinates are within
#'   the buffer radius. Returns \code{FALSE} and a Warning if the coordinates
#'   are outside the buffer.
#'
#' @importFrom sf st_as_sf st_buffer st_intersection st_transform
#' @importFrom dplyr %>% contains filter mutate select
#' @importFrom glue glue
#' @importFrom googlesheets4 gs4_deauth read_sheet
#'
#' @export

ss_check_station_radius <- function(
    log_coords,
    log_crs = 4617,
    station_coords = NULL,
    station_radius = 500
) {

  station_name <- log_coords$station

  if(is.null(station_coords)) {
    # link to the "STRING TRACKING" google sheet -
    googlesheets4::gs4_deauth()

    link <- "http://docs.google.com/spreadsheets/d/1a3QvJsvwr4dd64g3jxgewRtfutIpsKjT2yrMEAoxA3I/edit#gid=828367890"

    # read in the "Area Info" tab of the STRING TRACKING sheet
    station_coords <- googlesheets4::read_sheet(link, sheet = "Area Info") %>%
      filter(station == station_name) %>%
      select(station, latitude, longitude)
  }

  if (nrow(station_coords) > 1) {
    stop("More than one set of coordinates provided for station << ",
         station_name, " >>")
  }
  if (nrow(station_coords) < 1) {
    stop("Must provide coordinates for station << ", station_name, " >>")
  }

  station_coords_sf <- station_coords %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = log_crs)

  station_buffer <- st_buffer(station_coords_sf, dist = station_radius)

  log_coords_sf <- log_coords %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = log_crs)

  #check_location <- st_within(log_coords_sf, station_buffer)
  location_check <- suppressWarnings(
    st_intersection(log_coords_sf, station_buffer))


  if(nrow(location_check) == 0) {
    warning(
      glue("The coordinates for station << {station_name} >> are outside of the
           << {station_radius} m >> buffer from the official station location.")
    )
    return(FALSE)
  }

  return(TRUE)
}


#' Check station location is in the ocean (not on land)
#'
#' @inheritParams ss_check_station_radius
#' @param coast_shp Shapefile of the coast for comparison to the station
#'   location. The resolution of the shapefile can impact the results of this
#'   test. Coarse resolution may result in false positives for overlapping with
#'   land. If \code{NULL}, must be connected to the Perennia R drive.
#'
#' @return Logical value. Returns \code{TRUE} if the log coordinates are in the
#'   ocean. Returns \code{FALSE} and a Warning if the coordinates overlap with
#'   land.
#'
#' @importFrom sf st_as_sf st_intersection read_sf
#' @importFrom glue glue
#'
#' @export

ss_check_station_in_ocean <- function(
    log_coords,
    log_crs = 4617,
    coast_shp = NULL
) {

  station_name <- log_coords$station

  if(is.null(coast_shp)) {
    coast_shp <- read_sf(
      "R:/data_branches/water_quality/processing_resources/ns_coast/ns_coast.shp") %>%
      na.omit()
  }

  coast_shp <- coast_shp %>%
    st_transform(crs = log_crs)

  log_coords_sf <- log_coords %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = log_crs)

  overlap <- suppressWarnings(st_intersection(log_coords_sf, coast_shp))

  if(nrow(overlap) > 0) {
    warning(
      glue("The coordinates for station << {station_name} >> may be on land.")
    )
    return(FALSE)
  }

  return(TRUE)
}



ss_check_station_depl_vs_retrieval <- function(
    log_coords,
    log_crs,
    max_drift = 100
) {

  log_coords = data.frame(
    station = "Birchy Head",
    latitude = 44.56, longitude = -64.03,
    retrieval_latitude = 44.54, retrieval_longitude = -64.05
  )

  depl <- log_coords %>%
    select(latitude, longitude) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = log_crs)

  retrieval <- log_coords %>%
    select(latitude = retrieval_latitude, longitude = retrieval_longitude) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = log_crs)


  drift_distance <- st_distance(depl, retrieval)
  drift_distance <- drift_distance[[1]]


  if(drift_distance  > max_drift) {
    warning(
      glue("The deployment and retrieval coordinates for station << {station} >> are greater than
           {max_drift} m apart.")
    )
    return(FALSE)
  }

  return(TRUE)
  }






if(FALSE) {

  library(canadianmaps)
  library(ggplot2)
  library(ggspatial)
  library(sf)

  station_name <- "Birchy Head"
  station_radius = 500
  station_coords = NULL
  log_crs = 4617

  log_coords <- data.frame(latitude = 44.56975, longitude = -64.03448)


  station_coords_sf <- data.frame(latitude = 45.66, longitude =		-60.85) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = log_crs)

  dummy_coords <- data.frame(longitude = c(-62,  -61), latitude = c(45, 45.5)) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = log_crs)

  dummy_coords <- data.frame(longitude = c(-61.8,  -61.6), latitude = c(45.1, 45.25)) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = log_crs)

  ns <- filter(PROV, PT == "NS")
  ns <- ns %>%
    st_transform(crs = log_crs) %>%
    st_crop(dummy_coords)

  ggplot() +
    geom_sf(data = ns) +
    geom_sf(data = station_coords_sf, col = "red") +
    geom_sf(data = buffer, col = "blue", fill = NA) +
    annotation_scale()
}

