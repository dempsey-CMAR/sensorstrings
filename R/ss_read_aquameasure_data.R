#' @title Import data from aquaMeasure sensors
#'
#' @details The aquaMeasure data must be saved in a folder named aquaMeasure in
#'   .csv or .xlsx format.
#'
#'   The same data imported from .csv and .xlsx may have a string mismatch in
#'   the Timestamp(UTC) Component. This is because read_excel includes the
#'   seconds, but fread does not
#'
#' @param path File path to the aquaMeasure folder.
#'
#' @param file_name Name of the file to import, including file extension.
#'
#' @param force_POSIXct Logical argument indicating whether to convert the
#'   timestamp column to POSIXct. Timestamp messages "undefined" and "xs after
#'   startup (time not set)" will be coerced to \code{NA} without a warning.
#'
#' @return Returns a tibble of aquaMeasure data, with the same columns as in the
#'   original file.
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


ss_read_aquameasure_data <- function(path, file_name, force_POSIXct = TRUE) {

  # finish path
  path <- file.path(str_glue("{path}/aquaMeasure/{file_name}"))

  # extract the file extension
  file_type <- extract_file_extension(file_name)

  # csv file ----------------------------------------------------------------
  if (file_type == "csv") {
    dat <- data.table::fread(
      path,
      header = TRUE, data.table = FALSE, na.strings = ""
    ) %>%
      as_tibble()
  }

  # xlsx file --------------------------------------------------------------

  if (file_type == "xlsx") {
    dat <- read_excel(path, col_names = TRUE) %>%
      mutate(
        ROW = 1:n(),
        INDEX = suppressWarnings(is.na(as.numeric(`Timestamp(UTC)`)))
      )

    # this gave a Warning when tried using if_else()
    # timestamp message
    dat_char <- dat %>% filter(INDEX)

    # real timestamp
    dat_date <- dat %>%
      filter(!INDEX) %>%
      mutate(
        `Timestamp(UTC)` = janitor::convert_to_datetime(as.numeric(`Timestamp(UTC)`)),
        `Timestamp(UTC)` = as.character(`Timestamp(UTC)`)
      )

    # bind back togther
    dat <- rbind(dat_char, dat_date) %>%
      arrange(ROW) %>%
      select(-ROW, -INDEX)
  }

  # Export -----------------------------------------------------

  if (isTRUE(force_POSIXct)) {
    dat <- dat %>%
      mutate(
        `Timestamp(UTC)` = suppressWarnings(
          parse_date_time(`Timestamp(UTC)`, orders = c("Ymd HM", "Ymd HMS"))
        )
      )
  }
  dat
}
