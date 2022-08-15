
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

test_that("ss_read_hobo_data() report error for excel files",{

  expect_error(ss_read_hobo_data(path, "error.xls"))
  expect_error(ss_read_hobo_data(path, "error.xls"))

})


# ss_compile_hobo_data ----------------------------------------------------


test_that("ss_compile_hobo_data() returns correct classes", {

  expect_equal(class(hobo_trim$deployment_range), "character")
  expect_equal(class(hobo_trim$timestamp_utc), c("POSIXct", "POSIXt"))
  expect_equal(class(hobo_trim$sensor), "character")
  expect_equal(class(hobo_trim$depth), "numeric")
  expect_equal(class(hobo_trim$temperature_degree_C), "numeric")
  expect_equal(class(hobo_trim$dissolved_oxygen_mg_per_L), "numeric")
})



test_that("ss_compile_hobo_data() reads in all observations", {

  expect_equal(nrow(hobo_all), 49)
  expect_equal(nrow(hobo_trim), 36)

})








