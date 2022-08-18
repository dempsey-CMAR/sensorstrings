
# convert_timestamp_to_datetime() -----------------------------------------

test_that("convert_timestamp_to_datetime() works", {
  expect_equal(class(ts$timestamp_), c("POSIXct", "POSIXt"))
  expect_error(convert_timestamp_to_datetime(ts_error))
})


# extract_hobo_sn() -------------------------------------------------------

test_that("extract_hobo_sn() works", {
  expect_equal(extract_hobo_sn(hobo_colnames), 10755220)
  expect_error(extract_hobo_sn(hobo_colnames_error))
})


# extract_hobo_units() ----------------------------------------------------

test_that("extract_hobo_units() returns correct units", {
  expect_equal(hobo_units$units, c("utc", "degree_C", "mg_per_L"))
})


# make_column_names() -------------------------------------------------------

test_that("make_column_names() returns correct names", {
  expect_equal(
    new_hobo_colnames$col_name,
    c("timestamp_utc", "dissolved_oxygen_mg_per_L", "temperature_degree_C")
  )
})
