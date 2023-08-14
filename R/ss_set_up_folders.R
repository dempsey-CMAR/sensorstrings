#' Create folders for the raw sensor string data from a given deployment
#'
#' @param path File path to where the deployment folder should be created.
#'
#' @param station Station name.
#'
#' @param depl_date Deployment start date as a character string.
#'
#' @param sensor_folders Logical argument indicating whether to create the
#'   aquameasure, hobo, log, and vemco folders in the deployment folder. These
#'   folders may also be created using \code{ss_download_data()}.
#'
#' @return Creates the folder structure for storing raw sensor string data from
#'   a single deployment.
#'
#' @importFrom lubridate parse_date_time
#' @importFrom stringr str_detect str_to_lower str_replace_all
#'
#' @export

ss_set_up_folders <- function(
    path = NULL, station, depl_date, sensor_folders = FALSE) {
  parse_orders <- c("Ymd", "ymd", "dmY", "dmy", "mdY", "mdy")

  # will give an error if depl_date is not in the correct order
  depl_date_out <- as.character(
    suppressWarnings(parse_date_time(depl_date, orders = parse_orders))
  )

  if (is.na(depl_date_out)) {
    stop("depl_date << ", depl_date, " >> could not be converted to a date in format yyyy-mm-dd")
  }

  station_folders <- list.files(path)

  # ensure station is converted to snake case
  station_snake <- str_to_lower(station)
  station_snake <- str_replace_all(station_snake, " ", "_")

  # if the station folder does not exist, create it
  if (!any(str_detect(station_folders, station_snake))) {
    dir.create(paste0(path, "/", station_snake))

    message("Created folder << ", station_snake, " >> in <<", path, " >>")
  }

  path <- paste0(path, "/", station_snake)

  depl_folders <- list.files(path)

  new_folder <- paste(station_snake, depl_date_out, sep = "_")

  if (any(str_detect(depl_folders, new_folder))) {
    stop("Deployment folder << ", new_folder, " >> already exists in << ", path, " >>")
  }

  path <- paste0(path, "/", new_folder)

  dir.create(path)

  if (isTRUE(sensor_folders)) {
    dir.create(paste0(path, "/log"))
    dir.create(paste0(path, "/aquameasure"))
    dir.create(paste0(path, "/hobo"))
    dir.create(paste0(path, "/vemco"))
  }
}
