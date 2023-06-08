#' @title Extracts the extension of a file name
#'
#' @details Extracts the file extension from a character string using //. as the
#'   separator.
#'
#' @param file_name Character string of a file name or path. Must only include
#'   one ".", which is used as the separator.
#'
#' @importFrom tidyr separate

extract_file_extension <- function(file_name) {
  extension <- file_name %>%
    data.frame() %>%
    separate(col = 1, into = c(NA, "EXT"), sep = "\\.")

  extension$EXT
}


#' Generate file path and name to export compiled sensor string data
#'
#' @param dat Data frame of sensor string data in wide or long format. Must
#'   include columns \code{county}, \code{station}, and \code{deployment_range}.
#'
#' @param path Character string of the file path to the county folder.
#'
#' @param sub_folder Character string of the sub-folder name (inside county
#'   folder) where \code{dat} should be exported. Default is \code{sub-folder =
#'   "new"}.
#'
#' @param ext File extension. Default is \code{ext = "csv"}.
#'
#' @return A file path for exporting deployment data, including file name and
#'   extension.
#'
#' @importFrom dplyr distinct mutate
#' @importFrom stringr str_replace_all
#' @importFrom tidyr separate
#'
#' @export


ss_export_path <- function(
    dat, path = NULL, sub_folder = NULL, ext = "rds") {
  if (is.null(path)) {
    path <- "Y:/coastal_monitoring_program/data_branches/water_quality/processed_data/deployment_data"
  }

  if (is.null(sub_folder)) sub_folder <- "new"

  info <- dat %>%
    distinct(county, station, deployment_range) %>%
    separate("deployment_range", into = c("depl_date", NA, NA), sep = " ") %>%
    mutate(
      county = tolower(county),
      station = tolower(station),
      station = str_replace_all(station, " ", "_"),
      depl_date = as.character(as_date(depl_date))
    )

  file_name <- paste0(paste(info$station, info$depl_date, sep = "_"), ".", ext)

  path <- file.path(paste(path, info$county, sub_folder, sep = "/"))

  if (isFALSE(dir.exists(path))) {
    stop("File path << ", path, " >> does not exist.\nCan't export file << ", file_name, " >>")
  }

  file.path(paste(path, file_name, sep = "/"))
}


#' Generate file path to import raw sensor string deployment data
#'
#' @param path Character string of the file path to the station_folders folder.
#'
#' @param station Character string of the station name. Will be converted to
#'   lower case, and a space will be replaced with an underscore.
#'
#' @param depl_date Character string of the deployment data in the order
#'   yyyy-mm-dd.
#'
#' @return The file path for importing raw deployment data.
#'
#' @importFrom stringr str_replace_all
#'
#' @export

ss_import_path <- function(station, depl_date, path = NULL) {
  if (is.null(path)) {
    path <- "Y:/coastal_monitoring_program/data_branches/water_quality/station_folders"
  }

  station <- tolower(station)
  station <- str_replace_all(station, " ", "_")

  path <- paste(
    path, station,
    paste(station, depl_date, sep = "_"),
    sep = "/"
  )

  if (isFALSE(dir.exists(path))) {
    stop("File path << ", path, " >> does not exist. Check station spelling and deployment date")
  }

  file.path(path)
}
