#' @title Import data from aquaMeasure sensors
#'
#' @details The aquaMeasure data must be saved in a folder named aquameasure in
#'   .csv format.
#'
#' @param path File path to the aquaMeasure folder.
#'
#' @param file_name Name of the file to import, including file extension.
#'
#' @return Returns a data.frame of aquaMeasure data, with the same columns as in
#'   the original file.
#'
#' @author Danielle Dempsey
#'
#' @importFrom data.table fread
#' @importFrom dplyr %>% arrange as_tibble filter mutate n select
#' @importFrom janitor convert_to_datetime
#' @importFrom lubridate parse_date_time
#' @importFrom readxl read_excel
#' @importFrom stringr str_glue
#'
#' @export


ss_read_aquameasure_data <- function(path, file_name) {

  ext <- extract_file_extension(file_name)

  if(ext != "csv") stop(paste0("Cannot read file with extension: ", ext))

  # finish path
  path <- file.path(str_glue("{path}/{file_name}"))

  data.table::fread(path, header = TRUE, data.table = FALSE, na.strings = "")

}


#
##param force_POSIXct Logical argument indicating whether to convert the
#  timestamp column to POSIXct. Timestamp messages "undefined" and "xs after
# startup (time not set)" will be coerced to \code{NA} without a warning.
# # Export -----------------------------------------------------
#
# if (isTRUE(force_POSIXct)) {
#   dat <- dat %>%
#     mutate(
#       `Timestamp(UTC)` = suppressWarnings(
#         parse_date_time(`Timestamp(UTC)`, orders = c("Ymd HM", "Ymd HMS"))
#       )
#     )
# }
# dat
