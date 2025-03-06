# allow access to the google sheet
# googlesheets4::gs4_deauth()


# Common compile arguments ------------------------------------------------

path <- system.file("testdata", package = "sensorstrings")

#path_config <- paste0(system.file("testdata", package = "sensorstrings"), "/water_quality_configuration_table.xlsx")

deployment_dates <- data.frame(START = "2019-05-30", END = "2019-10-19")


# log ---------------------------------------------------------------------

# old log
log_old <- ss_read_log(path, parse = FALSE)
log_old2 <- ss_read_log(
  paste0(path, "/Log/Borgles_Island_2019-05-30_Log.xls" ), parse = FALSE
)

log_old_parse <- log_old %>% ss_parse_log()

# new log
log_new <- ss_read_log(paste0(path, "/new/new_log1.csv" ), parse = FALSE)

log_new_parse <- log_new %>% ss_parse_log(verbose = FALSE)


# aquameasure -------------------------------------------------------------

# ss_read_aquameasure_data ------------------------------------------------

path_am <- system.file("testdata/aquameasure", package = "sensorstrings")

am1 <- ss_read_aquameasure_data(path_am, "aquaMeasure-670364.csv")

am2 <- ss_read_aquameasure_data(paste0(path_am, "/aquaMeasure-670364.csv"))

# ss_compile_aquameasure_data ---------------------------------------------

sn_am <- data.frame(
  sensor_type = rep("aquaMeasure", 3),
  sensor_serial_number = c(670364, 680154, 675008),
  depth = c(1, 2, 3)
)

am_all <- ss_compile_aquameasure_data(
  path,
  sn_table = sn_am,
  deployment_dates = deployment_dates,
  trim = FALSE
)

## paths needed for dashboard
am_all2 <- ss_compile_aquameasure_data(
  path = c(paste0(path_am, "/aquaMeasure-670364.csv"),
           paste0(path_am, "/aquaMeasure-675008.csv"),
           paste0(path_am, "/aquaMeasure-680154.csv")),
  sn_table = sn_am,
  deployment_dates = deployment_dates,
  trim = FALSE
)

am_trim <- ss_compile_aquameasure_data(
  path,
  sn_table = sn_am,
  deployment_dates = deployment_dates,
  trim = TRUE
)


# HOBO --------------------------------------------------------------------

# ss_read_hobo_data -------------------------------------------------------

path_hobo <- system.file("testdata/Hobo", package = "sensorstrings")

hobo1 <- ss_read_hobo_data(path_hobo, "10755220.csv") %>%
  # the degree symbol was causing a problem
  dplyr::rename(temperature = 2)

hobo2 <- ss_read_hobo_data(path_hobo, "20827226.csv") %>%
  # the degree symbol was causing a problem
  dplyr::rename(temperature = 4)

hobo3 <- ss_read_hobo_data(paste0(path_hobo, "/20827226.csv")) %>%
  dplyr::rename(temperature = 4)

# # ss_compile_hobo_data ----------------------------------------------------

sn_hobo <- data.frame(
  sensor_type = c("HOBO", "hobo"),
  sensor_serial_number = c(10755220, 20827226),
  depth = c(4, 5)
)

hobo_all <- ss_compile_hobo_data(
  path,
  sn_table = sn_hobo,
  deployment_dates = deployment_dates,
  trim = FALSE
)

## paths needed for dashboard
hobo_all2 <- ss_compile_hobo_data(
  path = c(paste0(path_hobo, "/10755220.csv"),
           paste0(path_hobo, "/20827226.csv")),
  sn_table = sn_hobo,
  deployment_dates = deployment_dates,
  trim = FALSE
)

hobo_trim <- ss_compile_hobo_data(
  path,
  sn_table = sn_hobo,
  deployment_dates = deployment_dates,
)

# Vemco -------------------------------------------------------------------

# ss_read_vemco_data ------------------------------------------------

path_vem1 <- system.file("testdata/vemco", package = "sensorstrings")

vemco1 <- ss_read_vemco_data(path_vem1, "vemco-547109.csv")

vemco2 <- ss_read_vemco_data(paste0(path_vem1, "/vemco-547109.csv"))

# ss_compile_vemco_data ---------------------------------------------

sn_vem <- data.frame(
  sensor_type = "VR2AR",
  sensor_serial_number = "547109",
  depth = 6
)

vem_all <- ss_compile_vemco_data(
  path,
  sn_table = sn_vem,
  deployment_dates = deployment_dates,
  trim = FALSE
)

vem_all2 <- ss_compile_vemco_data(
  path = paste0(path_vem1, "/vemco-547109.csv"),
  sn_table = sn_vem,
  deployment_dates = deployment_dates,
  trim = FALSE
)

vem_trim <- ss_compile_vemco_data(
  path,
  sn_table = sn_vem,
  deployment_dates = deployment_dates,
  trim = TRUE
)

# make sure UTF-8 encoding works too
path_vem2 <- system.file("testdata/test7", package = "sensorstrings")

vem_trim2 <- ss_compile_vemco_data(
  path_vem2,
  sn_table = sn_vem,
  deployment_dates = deployment_dates,
  trim = TRUE
)

# ALL deployment data -----------------------------------------------------

depl_all <- ss_compile_deployment_data(path, trim = FALSE)

depl_trim <- ss_compile_deployment_data(path, trim = TRUE)

# helpers-compile ---------------------------------------------------------

# convert_timestamp_to_datetime -------------------------------------------

ts <- data.frame(
  timestamp_ = c(
    "22-08-16 3:04:00 PM",
    "22/09/16 3:05:01 PM",
    "2022-10-16 3:06:54 pm",
    "2022-11-16 15:07",
    "2022-12-16:08:14",
    "16-01-2022 15:09",
    "16-02-2022 15:10:50",
    "16-03-2022 3:11 AM",
    "16/04/2022 3:11:12 AM"
  )
) %>%
  convert_timestamp_to_datetime()

ts_error <- data.frame(timestamp_ = "08-22-2019 12:55")


# extract_hobo_sn ---------------------------------------------------------

hobo_colnames <- c(
  "Date Time, GMT+00:00",
  "Temp, °C (LGR S/N: 10755220, SEN S/N: 10755220)"
)

hobo_colnames_error <- c(
  "Date Time, GMT+00:00",
  "Temp, °C (LGR S/N: 10755220, SEN S/N: 123456)"
)


# extract_hobo_units ---------------------------------------------------

hobo_units <- ss_read_hobo_data(
  path = system.file("testdata/Hobo", package = "sensorstrings"),
  file_name = "20827226.csv"
) %>%
  extract_hobo_units()

# make_column_names -------------------------------------------------------

new_hobo_colnames <- make_column_names(hobo_units)

# ss_pivot ----------------------------------------------------------------

long_all <- ss_pivot_longer(depl_all)
wide_all <- ss_pivot_wider(long_all)
long_all2 <- ss_pivot_longer(wide_all)

long_trim <- ss_pivot_longer(depl_trim)
wide_trim <- ss_pivot_wider(long_trim)
long_trim2 <- ss_pivot_longer(depl_trim)

# convert coordinates -----------------------------------------------------

coords_ddm <- c("45 21.651", "61 24.407", "44 26.238", "64 15.038")
#coords_dd <- ss_coords_from_ddm_to_dd(coords_ddm)



