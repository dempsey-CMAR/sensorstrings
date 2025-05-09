# ss_read_vemco_data -------------------------------------------------------

test_that("ss_read_vemco_data() reads in correct classes", {
  expect_equal(class(vemco1$`Date and Time (UTC)`), "character")
  expect_equal(class(vemco1$Receiver), "character")

  expect_equal(class(vemco1$Description), "character")
  expect_equal(class(vemco1$Data), "character")
  expect_equal(class(vemco1$Units), "character")
})


test_that("ss_read_vemco_data() reads in all observations", {
  expect_equal(nrow(vemco1), 129)
})

test_that("ss_read_vemco_data() reads data when path includes file name", {
  expect_equal(vemco1, vemco2)
})


test_that("ss_read_vemco_data() report error for excel files", {
  expect_error(ss_read_vemco_data(path, "error.xls"))
  expect_error(ss_read_vemco_data(path, "error.xlsx"))
  expect_error(ss_read_vemco_data(paste0(path_vemco, "/12345678")))
})


# ss_compile_vemco_data ---------------------------------------------------

test_that("ss_compile_vemco_data() returns correct classes", {
  expect_equal(class(vem_all$deployment_range), "character")
  expect_equal(class(vem_all$timestamp_utc), c("POSIXct", "POSIXt"))
  expect_equal(class(vem_all$sensor_type), "character")
  expect_equal(class(vem_all$sensor_serial_number), "numeric")
  expect_equal(class(vem_all$sensor_depth_at_low_tide_m), "numeric")
  expect_equal(class(vem_all$sensor_depth_measured_m), "numeric")
  expect_equal(class(vem_all$temperature_degree_c), "numeric")
})

test_that("ss_compile_vemco_data() reads in all observations", {
  expect_equal(nrow(vem_all), 17)
  expect_equal(nrow(vem_all2), 17)
  expect_equal(nrow(vem_trim), 16)
  expect_equal(nrow(vem_trim2), 15)
})

test_that("ss_compile_vemco_data() fixes degree symbol for UTF-8 and ANSI encoding", {
  expect_true("temperature_degree_c" %in% colnames(vem_trim))
  expect_true("temperature_degree_c" %in% colnames(vem_trim2))
})

test_that("ss_compile_vemco_data() returns Error and Warnings", {
  expect_error(
    ss_compile_vemco_data(
      path = path,
      deployment_dates = deployment_dates,
      sn_table = data.frame(sensor = "VR2AR", serial = "123456", depth = 6)
    )
  )

  expect_error(
    ss_compile_vemco_data(
      path = path,
      deployment_dates = data.frame(START = "2020-05-30", END = "2020-10-19"),
      sn_table = sn_vem
    )
  )
})

