

# ss_read_hobo_data -------------------------------------------------------

path <- system.file("testdata/hobo", package = "sensorstrings")

hobo1 <- ss_read_hobo_data(path, "10755220.csv") %>%
  # I think the degree symbol was causing a problem
  dplyr::rename(temperature = 2)

hobo2 <- ss_read_hobo_data(path, "20827226.csv") %>%
  # I think the degree symbol was causing a problem
  dplyr::rename(temperature = 4)


# ss_compile_hobo_data
path2 <- system.file("testdata", package = "sensorstrings")

sn_table <- data.frame(sensor = c("HOBO", "HOBO"),
                       serial = c(10755220, 20827226),
                       depth = c(1, 2))

deployment_dates <- data.frame(START = "2019-05-30", END = "2019-10-19")


hobo_all <- ss_compile_hobo_data(
  path2,
  sn_table = sn_table,
  deployment_dates = deployment_dates,
  verbose = FALSE,
  trim = FALSE
)

hobo_trim <- ss_compile_hobo_data(
  path2,
  sn_table = sn_table,
  deployment_dates = deployment_dates,
  verbose = FALSE
)





