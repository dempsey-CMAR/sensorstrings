# January 15, 2021

# read in raw data, filter to make the file size smaller, and export to inst/extdata

# library(dplyr)
# library(readr)
# library(lubridate)
# library(stringr)

#' @importFrom data.table fwrite
#' @importFrom dplyr row_number

path <- system.file("data-raw", package = "sensorstrings")

# aquaMeasure -------------------------------------------------------------

aquameasure_raw <- ss_read_aquameasure_data(
  path = paste0(path, "/aquameasure"),
  file_name = "aquaMeasure-670364.csv"
)

aquameasure <- aquameasure_raw %>%
  filter(row_number() %% 40 == 0)

data.table::fwrite(aquameasure , file = "inst/extdata/aquameasure/aquameasure-670364.csv")


# hobo --------------------------------------------------------------------

hobo_raw <- ss_read_hobo_data(
  path = paste0(path, "/hobo"),
  file_name = "10755220.csv"
)

hobo <- hobo_raw %>%
  filter(row_number() %% 4 == 0)

data.table::fwrite(hobo , file = "inst/extdata/hobo/10755220.csv")


# Vemco -------------------------------------------------------------------

vemco_raw <- ss_read_vemco_data(
  path = paste0(path, "/vemco"),
  file_name = "vemco-547109.csv"
)

vemco <- vemco_raw %>%
  filter(dplyr::row_number() %% 15 == 0)

data.table::fwrite(vemco , file = "inst/extdata/vemco/vemco-547109.csv")


