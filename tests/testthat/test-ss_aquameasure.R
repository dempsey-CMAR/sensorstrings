
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
  expect_equal(class(am_all$depth), "numeric")
  expect_equal(class(am_all$temperature_degree_C), "numeric")
  expect_equal(class(am_all$dissolved_oxygen_percent_saturation), "numeric")
  expect_equal(class(am_all$salinity_psu), "numeric")
})

test_that("ss_compile_aquameasure_data() reads in all observations", {
  expect_equal(nrow(am_all), 34)
  expect_equal(nrow(am_trim), 28)
})

test_that("ss_compile_aquameasure_data() returns Errors and Warnings", {

  # when excel files found in aquameasure folder
  expect_warning(
    ss_compile_aquameasure_data(
      path2,
      sn_table = sn_am,
      deployment_dates = deployment_dates,
      verbose = TRUE
    )
  )

  # when sn_table has fewer rows than the number of csv files
  expect_error(
    ss_compile_aquameasure_data(
      path2,
      sn_table = data.frame(sensor = "aquameasure", serial = 10755220, depth = 1),
      deployment_dates = deployment_dates,
      verbose = FALSE
    )
  )

  # when sn_table has more rows than the number of csv files
  expect_error(
    ss_compile_aquameasure_data(
      path2,
      sn_table = data.frame(
        sensor = c("sens1", "sens2", "sens3"),
        serial = c(10755220, 20827226, 1234567),
        depth = c(1, 2, 3)
      ),
      deployment_dates = deployment_dates,
      verbose = FALSE
    )
  )

  # when file name doesn't match any entries in sn_table
  expect_error(
    ss_compile_aquameasure_data(
      path2,
      sn_table = data.frame(
        sensor = c("sens1", "sens2"),
        serial = c(1234, 5678),
        depth = c(1, 2)
      ),
      deployment_dates = deployment_dates,
      verbose = FALSE
    )
  )
})

