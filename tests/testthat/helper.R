# allow access to the google sheet
# googlesheets4::gs4_deauth()


# Common compile arguments ------------------------------------------------

path <- system.file("testdata", package = "sensorstrings")

path_config <- paste0(system.file("testdata", package = "sensorstrings"), "/water_quality_configuration_table.xlsx")

deployment_dates <- data.frame(START = "2019-05-30", END = "2019-10-19")


# aquameasure -------------------------------------------------------------

# ss_read_aquameasure_data ------------------------------------------------

path_am <- system.file("testdata/aquameasure", package = "sensorstrings")

am1 <- ss_read_aquameasure_data(path_am, "aquaMeasure-670364.csv")

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
  # I think the degree symbol was causing a problem
  dplyr::rename(temperature = 2)

hobo2 <- ss_read_hobo_data(path_hobo, "20827226.csv") %>%
  # I think the degree symbol was causing a problem
  dplyr::rename(temperature = 4)
#
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

hobo_trim <- ss_compile_hobo_data(
  path,
  sn_table = sn_hobo,
  deployment_dates = deployment_dates,
)

# Vemco -------------------------------------------------------------------

# ss_read_vemco_data ------------------------------------------------

path_vem1 <- system.file("testdata/vemco", package = "sensorstrings")

vemco1 <- ss_read_vemco_data(path_vem1, "vemco-547109.csv")

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

depl_all <- ss_compile_deployment_data(path, path_config, trim = FALSE)

depl_trim <- ss_compile_deployment_data(path, path_config, trim = TRUE)

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


# nsdfa tracking sheet ----------------------------------------------------

nsdfa <- ss_read_nsdfa_metadata(paste0(path, "/nsdfa_tracking_sheet.xlsx"))


# dissolved oxygen corrections --------------------------------------------

# temp <- seq(0, 30, 5)
# sal <- seq(0, 35, 5)
# press <- seq(0.5, 1.1, 0.2)
#
# temp_sal <- expand.grid(temp, sal) %>%
#   data.frame() %>%
#   rename(temperature_degree_c = Var1, salinity_psu = Var2)
#
# # read in usgs table for comparison with BK equation
# usgs <- read_csv(
#   paste0(path, "/dotables.csv"),
#   skip = 8,
#   show_col_types = FALSE
# ) %>%
#   rename(temperature_degree_c = 1)
# usgs <- usgs[] # to remove attributes from read_csv
#
# F_bk <- ss_dissolved_oxygen_salinity_correction(
#   temp_sal, method = "benson-krause"
# ) %>%
#   pivot_wider(names_from = "salinity_psu", values_from = "F_s")
#
# # GG results as of V0.1.0 (same as BK results until 4th digit)
# F_0 <- rep(1, 7)
#
# F_15 <- c(0.9005, 0.9048, 0.9087, 0.9122, 0.9154, 0.9183, 0.9210)
#
# F_30 <- c(0.8108, 0.8185, 0.8255, 0.8319, 0.8378, 0.8431, 0.8480)
#
#
# F_gg <- ss_dissolved_oxygen_salinity_correction(
#   temp_sal, method = "garcia-gordon"
# ) %>%
#   pivot_wider(names_from = "salinity_psu", values_from = "F_s")
#
#
# temp_press <- expand.grid(temp, press) %>%
#   data.frame() %>%
#   rename(temperature_degree_c = Var1, pressure_atm = Var2)
#
# # results as of V0.1.0
# F_p05 <- c(0.4972, 0.4959, 0.4941, 0.4916, 0.4884, 0.4840, 0.4783)
# F_p11 <- c(1.1005, 1.1008, 1.1011, 1.1016, 1.1023, 1.1032, 1.1043)
#
# F_p <- ss_dissolved_oxygen_pressure_correction(temp_press, sal = 0) %>%
#   select(temperature_degree_c, pressure_atm, F_p) %>%
#   pivot_wider(names_from = "pressure_atm", values_from = "F_p")
