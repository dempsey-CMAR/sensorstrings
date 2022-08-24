
# ss_read_vemco_data -------------------------------------------------------

test_that("ss_read_vemco_data() reads in correct classes", {
  expect_equal(class(vemco1$`Date and Time (UTC)`), "character")
  expect_equal(class(vemco1$Receiver), "character")

  expect_equal(class(vemco1$Description), "character")
  expect_equal(class(vemco1$Data), "character")
  expect_equal(class(vemco1$Units), "character")
})


test_that("ss_read_vemco_data() reads in all observations", {
  expect_equal(nrow(vemco1), 124)
})


test_that("ss_read_vemco_data() report error for excel files", {
  expect_error(ss_read_vemco_data(path, "error.xls"))
  expect_error(ss_read_vemco_data(path, "error.xlsx"))
})


# ss_compile_vemco_data ---------------------------------------------------

test_that("ss_compile_vemco_data() returns correct classes", {
  expect_equal(class(vem_all$deployment_range), "character")
  expect_equal(class(vem_all$timestamp_utc), c("POSIXct", "POSIXt"))
  expect_equal(class(vem_all$sensor), "character")
  expect_equal(class(vem_all$depth), "numeric")
  expect_equal(class(vem_all$temperature_degree_C), "numeric")
})

test_that("ss_compile_vemco_data() reads in all observations", {
  expect_equal(nrow(vem_all), 16)
  expect_equal(nrow(vem_trim), 15)
})


# test_that("ss_compile_vemco_data() returns Errors and Warnings", {
#
#   # when excel files found in vemco folder
#   expect_warning(
#     ss_compile_vemco_data(
#       path, sn_table = sn_vem, deployment_dates = deployment_dates,
#       trim = FALSE, verbose = TRUE
#     )
#   )
#
#   # when serial number in data file does not match sn_table
#   expect_error(
#     ss_compile_vemco_data(
#       path,
#       sn_table = data.frame(sensor = "sens1", serial = 1234, depth = 5),
#       deployment_dates = deployment_dates,
#       verbose = FALSE
#     )
#   )
#
# })
