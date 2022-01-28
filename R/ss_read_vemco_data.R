#' @title Import data from Vemco sensors
#'
#' @details The Vemco data must be saved in a folder named vemco in
#'   .csv or .xlsx format.
#'
#'   The same data imported from .csv and .xlsx may have a string mismatch in
#'   the Data Component. This is because read_excel includes the
#'   more decimal places than fread.
#'
#' @param path File path to the Vemco folder.
#'
#' @param file_name Name of the file to import, including file extension.
#'
#' @return Returns a tibble of Vemco data, with the same columns as in the
#'   original file.
#'
#' @author Danielle Dempsey
#'
#' @importFrom data.table fread
#' @importFrom dplyr %>%  as_tibble
#' @importFrom janitor convert_to_datetime
#' @importFrom lubridate parse_date_time
#' @importFrom readxl read_excel
#' @importFrom stringr str_glue
#'
#' @export


ss_read_vemco_data <- function(path, file_name){

  # finish path
  path <- file.path(str_glue("{path}/Vemco/{file_name}"))

  # extract the file extension
  file_type <- extract_file_extension(file_name)

  # use appropriate function to import data
  if(file_type == "csv") {

    dat <- data.table::fread(
      path, header = TRUE, data.table = FALSE, na.strings = ""
    ) %>%
      as_tibble()

  }

  if(file_type == "xlsx") {

    dat <- read_excel(path, col_names = TRUE)

  }


# Export -----------------------------------------------------

  dat

}

