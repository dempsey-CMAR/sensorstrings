# ss_read_hobo_data -------------------------------------------------------

test_that("ss_read_hobo_data() reads in correct classes", {
  expect_equal(class(hobo_ph1$`Date-Time (ADT)`), "character")
  expect_equal(class(hobo_ph1$temperature), "numeric")
  expect_equal(class(hobo_ph1$`pH   (pH)`), "numeric")
})


test_that("ss_read_hobo_data() reads in all observations", {
  expect_equal(nrow(hobo_ph1), 436)
})

# ss_compile_hobo_ph_data ----------------------------------------------------

test_that("ss_compile_hobo_ph_data() returns correct classes", {
  expect_equal(class(hobo_ph_all$deployment_range), "character")
  expect_equal(class(hobo_ph_all$timestamp_adt), c("POSIXct", "POSIXt"))
  expect_equal(class(hobo_ph_all$sensor_type), "character")
  expect_equal(class(hobo_ph_all$sensor_serial_number), "numeric")
  expect_equal(class(hobo_ph_all$sensor_depth_at_low_tide_m), "numeric")
  expect_equal(class(hobo_ph_all$temperature_degree_c), "numeric")
  expect_equal(class(hobo_ph_all$ph_ph), "numeric")
})


test_that("ss_compile_hobo_ph_data() reads in all observations", {
  expect_equal(nrow(hobo_ph_all), 432)

})

test_that("ss_compile_hobo_ph_data() returns error if file name does not match entries in sn_table", {
  expect_error(
    ss_compile_hobo_ph_data(
      path,
      sn_table = data.frame(
        sensor = "hobo_ph",
        serial = 1234,
        depth = 2
      ),
      deployment_dates = deployment_dates
    )
  )
})

test_that("ss_compile_hobo_ph_data() returns Error if trimming removes all rows", {
  expect_error(
    suppressWarnings(
      ss_compile_hobo_ph_data(
        path = path,
        deployment_dates = data.frame(START = "2020-05-30", END = "2020-10-19"),
        sn_table = sn_hobo_ph
      )
    )
  )
})
