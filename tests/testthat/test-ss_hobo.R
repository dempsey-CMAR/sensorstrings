
# ss_read_hobo_data -------------------------------------------------------

test_that("ss_read_hobo_data() reads in correct classes", {
  expect_equal(class(hobo1$`Date Time, GMT+00:00`), "character")
  expect_equal(class(hobo1$temperature), "numeric")

  expect_equal(class(hobo2$`Date Time, GMT+00:00`), "character")
  expect_equal(class(hobo2$`DO conc, mg/L (LGR S/N: 20827226, SEN S/N: 20827226)`), "numeric")
  expect_equal(class(hobo2$temperature), "numeric")
})


test_that("ss_read_hobo_data() reads in all observations", {
  expect_equal(nrow(hobo1), 22)
  expect_equal(nrow(hobo2), 27)
})


test_that("ss_read_hobo_data() report error for excel files", {
  expect_error(ss_read_hobo_data(path, "error.xls"))
  expect_error(ss_read_hobo_data(path, "error.xlsx"))
})



# ss_compile_hobo_data ----------------------------------------------------

# test_that("ss_compile_hobo_data() returns correct classes", {
#   expect_equal(class(hobo_trim$deployment_range), "character")
#   expect_equal(class(hobo_trim$timestamp_utc), c("POSIXct", "POSIXt"))
#   expect_equal(class(hobo_trim$sensor), "character")
#   expect_equal(class(hobo_trim$sensor_depth_at_low_tide_m), "numeric")
#   expect_equal(class(hobo_trim$temperature_degree_C), "numeric")
#   expect_equal(class(hobo_trim$dissolved_oxygen_mg_per_L), "numeric")
# })
#
#
# test_that("ss_compile_hobo_data() reads in all observations", {
#   expect_equal(nrow(hobo_all), 49)
#   expect_equal(nrow(hobo_trim), 36)
# })
#
# test_that("ss_compile_hobo_data() returns error if file name does not match entries in sn_table",{
#
#   expect_error(
#     ss_compile_hobo_data(
#       path,
#       sn_table = data.frame(
#         sensor = c("HOBO", "HOBO"),
#         serial = c(1234, 5678),
#         depth = c(1, 2)
#       ),
#       deployment_dates = deployment_dates
#     )
#   )
# })
#
# test_that("ss_compile_hobo_data() returns Error if trimming removes all rows", {
#   expect_error(
#     ss_compile_hobo_data(
#       path = path,
#       deployment_dates = data.frame(START = "2020-05-30", END = "2020-10-19"),
#       sn_table = sn_hobo
#     )
#   )
# })

