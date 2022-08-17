

test_that("convert_timestamp_to_datetime() works", {

  expect_equal(class(ts$timestamp_), c("POSIXct", "POSIXt"))
  expect_error(convert_timestamp_to_datetime(ts_error))

})


test_that("extract_hobo_sn() works", {

  expect_equal(extract_hobo_sn(hobo_colnames), 10755220)
  expect_error(extract_hobo_sn(hobo_colnames_error))

})

test_that("ss_extract_hobo_units() returns correct units", {

  expect_equal(hobo_units$units, c("utc", "degree_C", "mg_per_L"))

})
