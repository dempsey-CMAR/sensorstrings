
test_that("ss_set_up_folders() returns expected Errors", {
  expect_error(
    ss_set_up_folders(station = "test", depl_date = "not a date")
  )
  expect_error(
    ss_set_up_folders(path = system.file("testdata", package = "sensorstrings"), station = "test1", depl_date = "2022-08-31")
  )
})

test_that("ss_set_up_folders() creates expected folders", {

  expect_message(
    ss_set_up_folders(path = path, station = "station name", depl_date = "2022-08-31")
  )

  expect_true(
    dir.exists(paste0(path, "/station name/station name_2022-08-31/aquameasure"))
  )
  expect_true(
    dir.exists(paste0(path, "/station name/station name_2022-08-31/hobo"))
  )
  expect_true(
    dir.exists(paste0(path, "/station name/station name_2022-08-31/log"))
  )
  expect_true(
    dir.exists(paste0(path, "/station name/station name_2022-08-31/vemco"))
  )

  # remove the folders so test will work again next time
  unlink(paste0(path, "/station name"), recursive = TRUE)
})


