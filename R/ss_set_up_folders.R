
# path <- "C:/Users/Danielle Dempsey/Desktop/Data_Strings"
#
# station <- "Madeline Point"
#
# station <- "test 1"
# depl_date <- "2019-04-25"
 #depl_date <- "19-04-25"
 #depl_date <- "25-06-2019"
#depl_date <- "not a date"

 #ss_set_up_folders(station = "test", depl_date = Sys.Date())


#' Create folders for the raw sensor string data from a given deployment
#'
#' @param path File path to where the deployment folder should be created.
#'
#' @param station Station name.
#'
#' @param depl_date Deployment start date.
#'
#' @param sensor_folders Logical argument indicating whether to create the
#'   aquameasure, hobo, log, and vemco folders in the deployment folder. These
#'   folder may also be created using \code{ss_download_data()}.
#'
#' @return Creates the folder structure for storing raw sensor string data from
#'   a single deployment.
#'
#' @importFrom lubridate parse_date_time
#' @importFrom stringr str_detect
#'
#' @export

ss_set_up_folders <- function(
  path = NULL, station, depl_date, sensor_folders = FALSE
) {

  parse_orders <- c("Ymd", "ymd","dmY", "dmy", "mdY", "mdy")

  # will give an error if depl_date is not in the correct order
  depl_date_out <- as.character(
    suppressWarnings(parse_date_time(depl_date, orders = parse_orders))
  )

  if (is.na(depl_date_out)) {
    stop("depl_date << ", depl_date, " >> could not be converted to a date in format yyyy-mm-dd")
  }

  station_folders <- list.files(path)

  # if the station folder does not exist, create it
  if (!any(str_detect(station_folders, station))) {
    dir.create(paste0(path, "/", station))

    message("Created folder << ", station, " >> in <<", path, " >>")
  }

  path <- paste0(path, "/", station)

  depl_folders <- list.files(path)

  new_folder <- paste(station, depl_date_out, sep = "_")

  if (any(str_detect(depl_folders, new_folder))) {
    stop("Deployment folder << ", new_folder, " >> already exists in << ", path, " >>")
  }

  path <- paste0(path, "/", new_folder)

  dir.create(path)

  if(isTRUE(sensor_folders)) {
    dir.create(paste0(path, "/log"))
    dir.create(paste0(path, "/aquameasure"))
    dir.create(paste0(path, "/hobo"))
    dir.create(paste0(path, "/vemco"))
  }
}

