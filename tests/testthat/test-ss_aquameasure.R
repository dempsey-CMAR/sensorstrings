
# ss_read_aquameasure_data -------------------------------------------------------

test_that("ss_read_aquameasure_data() reads in correct classes", {
  expect_equal(class(am1$`Timestamp(UTC)`), "character")
  expect_equal(class(am1$`Time Corrected(seconds)`), "integer")
  expect_equal(class(am1$Sensor), "character")
  expect_equal(class(am1$`Record Type`), "character")
  expect_equal(class(am1$`Dissolved Oxygen`), "numeric")
  expect_equal(class(am1$Temperature), "numeric")
  expect_equal(class(am1$`Device Tilt`), "numeric")
  expect_equal(class(am1$`Battery Voltage`), "numeric")
})

test_that("ss_read_aquameasure_data() reads in all observations", {
  expect_equal(nrow(am1), 47)
})

test_that("ss_read_aquameasure_data() gives an error when file extension is not csv", {
  expect_error(ss_read_aquameasure_data(path, "1234567"))
})


# ss_compile_aquameasure_data ---------------------------------------------

test_that("ss_compile_aquameasure_data() returns correct classes", {
  expect_equal(class(am_all$deployment_range), "character")
  expect_equal(class(am_all$timestamp_utc), c("POSIXct", "POSIXt"))
  expect_equal(class(am_all$sensor), "character")
  expect_equal(class(am_all$sensor_serial_number), "numeric")
  expect_equal(class(am_all$sensor_depth_at_low_tide_m), "numeric")
  expect_equal(class(am_all$sensor_depth_measured_m), "numeric")
  expect_equal(class(am_all$temperature_degree_C), "numeric")
  expect_equal(class(am_all$dissolved_oxygen_percent_saturation), "numeric")
  expect_equal(class(am_all$salinity_psu), "numeric")
})

test_that("ss_compile_aquameasure_data() reads in all observations", {
  expect_equal(nrow(am_all), 64)
  expect_equal(nrow(am_trim), 55)
})

test_that("ss_compile_aquameasure_data() returns Error if trimming removes all rows", {
  expect_error(
    ss_compile_aquameasure_data(
      path = path,
      deployment_dates = data.frame(START = "2020-05-30", END = "2020-10-19"),
      sn_table = sn_am
    )
  )
})


