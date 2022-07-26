#' @title Import data from HOBO and TidbiT sensors
#'
#' @param path File path to the hobo file.
#'
#' @param file_name Name of the file to import, including file extension.
#'
#' @return Returns a tibble of HOBO data, with the same columns as in the
#'   original file.
#'
#' @author Danielle Dempsey
#'
#' @importFrom data.table fread
#' @importFrom stringr str_glue
#'
#' @export


ss_read_hobo_data <- function(path, file_name) {

  # finish path
  path <- file.path(str_glue("{path}/{file_name}"))

  # read in data
  # start with row that includes the "Date" header
  data.table::fread(path, skip = "Date")

}
