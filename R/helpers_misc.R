#' @title Extracts the extension of a file name
#'
#' @details Extracts the file exension from a character string using //. as the
#'  separator.
#'
#' @param file_name Character string of a file name. Must only include one ".",
#'  which is used as the seprator.
#'
#' @export
#'
#' @importFrom tidyr separate

extract_file_extension <- function(file_name){

  extension <- file_name %>%
    data.frame() %>%
    separate(col = 1, into = c(NA, "EXT"), sep = "\\.")

  extension$EXT
}

# convert_timestamp_to_datetime() -----------------------------------------

#' @importFrom janitor convert_to_datetime
#' @importFrom lubridate parse_date_time

# function to convert the timestamp to a POSIXct object
# if the date can be converted to class numeric, then it was stored as a number in Excel
## and we have to use janitor::convert_to_datetime to convert to POSIXct.
# Otherwise the date should be a character string that can be converted to POSIXct using
## lubridate::parse_date_time()

convert_timestamp_to_datetime <- function(sensor.data){

  date_format <- sensor.data$TIMESTAMP[1]  # first datetime value; use to check the format

  # if saved as a number in Excel
  if(!is.na(suppressWarnings(as.numeric(date_format)))) {

    sensor.data <- sensor.data %>%
      mutate(TIMESTAMP = janitor::convert_to_datetime(as.numeric(TIMESTAMP)))

  } else{

    # if saved as a character string in Excel
    parse.orders <- c("ymd IMS p", "Ymd IMS p",
                      "Ymd HM", "Ymd HMS",
                      "dmY HM", "dmY HMS",
                      "dmY IM p", "dmY IMS p")

    if(!is.na(suppressWarnings(parse_date_time(date_format, orders = parse.orders)))){

      sensor.data <- sensor.data %>%
        mutate(TIMESTAMP = lubridate::parse_date_time(TIMESTAMP, orders = parse.orders))

    } else {

      # Error message if the date format is incorrect
      stop("Timestamp is not in a format recognized by the strings package.
           See help files for more information.")
    }
  }

  sensor.data
}
