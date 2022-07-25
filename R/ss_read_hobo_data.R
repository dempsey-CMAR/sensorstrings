#' @title Import data from HOBO and TidbiT sensors
#'
#' @details
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
#' @importFrom dplyr %>% as_tibble
#' @importFrom janitor convert_to_datetime
#' @importFrom lubridate as_datetime
#' @importFrom readxl read_excel
#' @importFrom stringr str_detect str_glue
#'
#' @export


ss_read_hobo_data <- function(path, file_name) {

  # finish path
  path <- file.path(str_glue("{path}/{file_name}"))

  # extract the file extension
  file_type <- extract_file_extension(file_name)

  # csv file ----------------------------------------------------------------
  if (file_type == "csv") {
    dat <- data.table::fread(
      path,
      header = TRUE, data.table = FALSE, na.strings = ""
    ) %>%
      as_tibble()

    # need to include the index column because data manipulation is based on
    # column numbers
    if (colnames(dat)[1] != "#") {
      stop(str_glue("Could not import HOBO file {path}.
                \nHINT: Re-export file from Hoboware.
                Make sure to select \'Include line number column\' "))
    }

    # might need to switch this to parse_datetime
    # maybe do something fancy so not using column index
    dat[[2]] <- as_datetime(dat[[2]])
  }

  # xlsx file --------------------------------------------------------------

  # read in all cols as text to avoid errors in columns with few entries
  if (file_type == "xlsx") {

    # check if first row is "Plot Title"
    skip_check <- read_excel(path, range = "A1", col_names = "CHECK")

    if (skip_check$CHECK == "#") {
      dat <- read_excel(path, col_names = TRUE, col_types = "text")
    } else if (str_detect(skip_check$CHECK, "Plot Title")) {
      dat <- read_excel(path, col_names = TRUE, col_types = "text", skip = 1)
    } else {
      stop(str_glue("Could not import HOBO file {path}.
                \nHINT: Re-export file from Hoboware.
                Make sure to select \'Include line number column\' "))
    }

    # convert index columns to numeric
    dat[[1]] <- as.numeric(dat[[1]])

    # convert Excel numeric date to POSIXct
    dat[[2]] <- janitor::convert_to_datetime(as.numeric(dat[[2]]))
  }

  # Clean up and export -----------------------------------------------------

  # clean up the DO and/or Temp columns & convert to numeric
  col_names <- colnames(dat)

  if (isTRUE(str_detect(col_names[3], "DO|Temp"))) {
    dat[[3]] <- round(as.numeric(dat[[3]]), digits = 3)
  }
  if (isTRUE(str_detect(col_names[4], "DO|Temp"))) {
    dat[[4]] <- round(as.numeric(dat[[4]]), digits = 3)
  }

  dat
}
