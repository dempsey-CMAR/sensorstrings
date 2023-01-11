
# set_up_compile ----------------------------------------------------------
test_that("set_up_compile() returns Errors and Warnings", {
  # when no csv file in folder
  expect_error(
    set_up_compile(
      path = path,
      sn_table = data.frame(sensor = "test1", serial = 123456, depth = 1),
      deployment_dates = data.frame(START = "2019-05-30", END = "2019-10-19"),
      sensor_make = "test1"
    )
  )

  # when xlsx file
  expect_error(
    set_up_compile(
      path = path,
      sn_table = data.frame(sensor = "test1", serial = 123456, depth = 1),
      deployment_dates = data.frame(START = "2019-05-30", END = "2019-10-19"),
      sensor_make = "test2"
    )
  )

  # when xls file
  expect_error(
    set_up_compile(
      path = path,
      sn_table = data.frame(sensor = "test1", serial = 123456, depth = 1),
      deployment_dates = data.frame(START = "2019-05-30", END = "2019-10-19"),
      sensor_make = "test3"
    )
  )

  # when two csv files in vemco folder
  expect_error(
    set_up_compile(
      path = paste0(path, "/test4"),
      sn_table = data.frame(sensor = "vemco", serial = 123456, depth = 1),
      deployment_dates = data.frame(START = "2019-05-30", END = "2019-10-19"),
      sensor_make = "vemco"
    )
  )

  # when sn_table has fewer rows than the number of csv files
  expect_warning(
    set_up_compile(
      path = path,
      sn_table = data.frame(sensor = "test5", serial = 123456, depth = 1),
      deployment_dates = data.frame(START = "2019-05-30", END = "2019-10-19"),
      sensor_make = "test5"
    )
  )

  # when sn_table has more rows than the number of csv files
  expect_warning(
    set_up_compile(
      path = path,
      sn_table = data.frame(
        sensor = c("test5", "test5", "test5"),
        serial = c(123456, 234567, 345678),
        depth = c(1, 2, 3)
      ),
      deployment_dates = data.frame(START = "2019-05-30", END = "2019-10-19"),
      sensor_make = "test5"
    )
  )
})


# check_n_rows() ------------------------------------------------------------

# could use snapshot test here
test_that("check_n_rows() returns an Error", {
  expect_error(
    check_n_rows(dat = data.frame(), file_name = "fake-file", trimmed = FALSE)
  )
  expect_error(
    check_n_rows(dat = data.frame(), file_name = "fake-file", trimmed = TRUE)
  )
})

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
  expect_equal(hobo_units$units, c("utc", "degree_c", "mg_per_l"))
})


# make_column_names() -------------------------------------------------------

test_that("make_column_names() returns correct names", {
  expect_equal(
    new_hobo_colnames$col_name,
    c("timestamp_utc", "dissolved_oxygen_uncorrected_mg_per_l", "temperature_degree_c")
  )
})
