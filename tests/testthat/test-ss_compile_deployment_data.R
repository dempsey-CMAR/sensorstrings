# ss_compile_deployment_data() ---------------------------------------------------

test_that("ss_compile_deployment_data() returns correct classes", {
  expect_equal(class(depl_all$deployment_range), "character")
  expect_equal(class(depl_all$sensor_type), "character")
  expect_equal(class(depl_all$sensor_serial_number), "numeric")
  expect_equal(class(depl_all$timestamp_utc), c("POSIXct", "POSIXt"))
  expect_equal(class(depl_all$sensor_depth_at_low_tide_m), "numeric")
  expect_equal(class(depl_all$sensor_depth_measured_m), "numeric")
  expect_equal(class(depl_all$dissolved_oxygen_percent_saturation), "numeric")
  expect_equal(class(depl_all$dissolved_oxygen_uncorrected_mg_per_l), "numeric")
  expect_equal(class(depl_all$temperature_degree_c), "numeric")
  expect_equal(class(depl_all$salinity_psu), "numeric")
})


test_that("ss_compile_deployment_data() reads in all observations", {
  expect_equal(
    nrow(depl_all),
    sum(nrow(am_all), nrow(hobo_all), nrow(hobo_ph_all), nrow(vem_all))
  )
  expect_equal(
    nrow(depl_trim),
    sum(nrow(am_trim), nrow(hobo_trim), nrow(vem_trim)) # don't include hobo_ph because dates are different
  )
})
