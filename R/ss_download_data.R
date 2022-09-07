
# how will this function work with ss_set_up_folders() ?
# run ss_set_up folders from here??
# then don't make folders that will be empty
# should there be separate sub-functions for each folder type?
# or just loop over the folders found in the deployment file and purrr over the files?
# how to test this function?
# might need to set working directory for each file downloaded...
# what about other folders in the Google drive? Maybe print a message if found

#' Download deployment data from Google drive to the shared Perennia drive
#'
#' @param path File path to the deployment folder (as created by
#'   \code{ss_set_up_folders()}).
#'
#' @param station Station name. Should match the name of a folder on the CMAR
#'   Google drive, in CMAR - Coastal Monitoring Program/CMP Station Data
#'
#' @param depl_date Deployment date. Should be in the name of one of the
#'   deployment folders.
#'
#' @return Downloads files from Google drive to the corresponding folder on the
#'   Perennia drive for processing.
#'
#' @importFrom dplyr %>% filter
#' @importFrom googledrive as_id drive_find drive_ls drive_download
#' @importFrom purrr walk2
#' @importFrom stringr str_detect
#'
#' @export

#
# station <- "Bald Rock"
# depl_date <- "2020-10-20"
#
# path <- "C:/Users/Danielle Dempsey/Desktop/Data_Strings"

ss_download_data <- function(path, station, depl_date) {

  # create aquameasure, hobo, log, and vemco folders in path/station/station_depl-date
  ss_set_up_folders(path, station, depl_date)

  # set path to the new deployment folder
  path <- paste(path, station, paste0(station, "_", depl_date), sep = "/")

  # find the deployment folder on the Google drive
  depl_folder <- drive_find(
    type = "folder", pattern = "CMP Station Data", n_max = 1
    ) %>%
    drive_ls() %>%
    filter(name == station) %>%
    drive_ls() %>%
    filter(str_detect(name, depl_date)) %>%
    drive_ls()

  # look inside each Google drive folder and download the data to the appropriate folder
  for (i in 1:nrow(depl_folder)) {

    data_folder <- depl_folder[i, ]

    # list files in data_folder - all will be downloaded
    data_files <- data_folder %>%
      drive_ls() %>%
      mutate(
        path = paste(path, tolower(data_folder$name), name, sep = "/")
      )

    # create local folder to store data
    if(!(data_folder$name %in% list.files(path))) {
      dir.create(paste(path, tolower(data_folder$name), sep = "/"))
    }

    # download the data
    walk2(
      data_files$id, data_files$path,
      ~ drive_download(as_id(.x), path = .y, overwrite = TRUE)
    )
  }
}
