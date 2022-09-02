
# how will this function work with ss_set_up_folders() ?
# run ss_set_up folders from here??
# then don't make folders that will be empty
# should there be separate sub-functions for each folder type?
# or just loop over the folders found in the deployment file and purrr over the files?
# how to test this function?
# might need to set working directory for each file downloaded...

#' Download deployment data from Google drive to the shared Perennia drive
#'
#' @param download_path File path to the deployment folder (as created by
#'   \code{ss_set_up_folders()}).
#'
#' @param station Station name. Should match the name of a folder on the CMAR
#'   Google drive, in CMAR - Coastal Monitoring Program/CMP Station Data
#'
#' @param depl_date Deployment date. Should be in the name of one of the
#'   deployment folders
#'
#' @return Downloads files from Google drive to the corresponding folder on the
#'   Perennia drive for processing.
#'
#' @importFrom dplyr %>% filter
#' @importFrom googledrive drive_find drive_ls drive_download
#' @importFrom stringr str_detect
#'
#' @export


# station <- "Bald Rock"
# depl_date <- "2020-10-20"

ss_download_data <- function(download_path, station, depl_date) {


  depl_folder <- drive_find(
    type = "folder", pattern = "CMP Station Data", n_max = 1
    ) %>%
    drive_ls() %>%
    filter(name == station) %>%
    drive_ls() %>%
    filter(str_detect(name, depl_date))

  # x <- drive_ls(depl_folder) %>%
  #   filter(str_detect(name, regex("hobo", ignore_case = TRUE))) %>%
  #   drive_ls()
  #
  # drive_download(x[1,])

}
