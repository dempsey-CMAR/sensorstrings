# January 15, 2021

# read in raw data, filter to make the file size smaller, and export to inst/extdata

# library(dplyr)
# library(readr)
# library(lubridate)
# library(stringr)

#' @importFrom here here
#' @importFrom data.table fwrite
#' @importFrom dplyr row_number

#path <- system.file("data-raw", package = "sensorstrings")
path <- here()

# aquaMeasure -------------------------------------------------------------

aquameasure_raw <- ss_read_aquameasure_data(
  path = paste0(path, "/data-raw/aquameasure"),
  file_name = "aquaMeasure-670364.csv"
)

aquameasure <- aquameasure_raw %>%
  dplyr::filter(dplyr::row_number() %% 40 == 0)

data.table::fwrite(aquameasure , file = "inst/extdata/aquameasure/aquameasure-670364.csv")


# hobo --------------------------------------------------------------------

hobo_raw <- ss_read_hobo_data(
  path = paste0(path, "/data-raw/hobo"),
  file_name = "10755220.csv"
)

hobo <- hobo_raw %>%
  dplyr::filter(dplyr::row_number() %% 4 == 0)

data.table::fwrite(hobo , file = "inst/extdata/hobo/10755220.csv")


# Vemco -------------------------------------------------------------------

vemco_raw <- ss_read_vemco_data(
  path = paste0(path, "/data-raw/vemco"),
  file_name = "vemco-547109.csv"
)

vemco <- vemco_raw %>%
  dplyr::filter(dplyr::row_number() %% 15 == 0)

data.table::fwrite(vemco , file = "inst/extdata/vemco/vemco-547109.csv")


