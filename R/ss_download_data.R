#' Download deployment data from Google drive to the shared CMAR server
#'
#' @param path File path to the deployment folder on the CMAR server (as created
#'   by \code{ss_set_up_folders()}).
#'
#' @param station Station name. Should match the name of a folder on the CMAR
#'   Google drive, in CMAR - Coastal Monitoring Program/CMP Station Data
#'
#' @param depl_date Deployment date (character string). Should be in the name of
#'   one of the deployment folders.
#'
#' @return Downloads files from Google drive to the corresponding folder on the
#'   CMAR server.
#'
#' @importFrom dplyr %>% filter
#' @importFrom googledrive as_id drive_find drive_ls drive_download
#' @importFrom purrr walk2
#' @importFrom stringr str_detect str_to_lower str_replace_all
#'
#' @export

ss_download_data <- function(path, station, depl_date) {
  # create the station and deployment folders
  ss_set_up_folders(path, station, depl_date)

  # ensure station is converted to snake case
  station_snake <- str_to_lower(station)
  station_snake <- str_replace_all(station_snake, " ", "_")

  # set path to the new deployment folder
  path <- paste(
    path, station_snake, paste0(station_snake, "_", depl_date),
    sep = "/"
  )

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

    dir.create(paste(path, tolower(data_folder$name), sep = "/"))

    # download the data
    walk2(
      data_files$id, data_files$path,
      ~ drive_download(as_id(.x), path = .y, overwrite = TRUE)
    )
  }
}
