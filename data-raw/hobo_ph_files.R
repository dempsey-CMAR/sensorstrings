path <- system.file("data-raw/hobo_ph", package = "sensorstrings")
file_name <- "22058696 2024-10-28 14_01_23 ADT (Data ADT).csv"

path <- paste(path, file_name, sep = "/")

sn_table <- data.frame(
  sensor_type = "hobo_ph",
  sensor_serial_numbner = 22058696,
  sensor_depth_m = 2
  )

deployment_dates <- data.frame(
  start = as_date("2024-10-25"),
  end = as_date("2024-10-28")
)
sensor_make <- "hobo_ph"


dat <- ss_compile_hobo_ph_data(path, sn_table, deployment_dates)



sn_table <- data.frame(
  log_sensor = c("hobo_ph", "hobo"),
  sensor_serial_number = c(22058696, 123456),
  sensor_depth_m = c(2, 5)
)
