
test_that("ss_read_log() returns Errors and Warnings", {

  # 2 files in log folder
  expect_error(ss_read_log(paste0(path, "/test1")))

  # multiple deployment dates
  expect_warning(ss_read_log(paste0(path, "/test2")))

  # multiple retrieval dates
  expect_warning(ss_read_log(paste0(path, "/test3")))

  # deployment dates are in wrong order
  expect_error(ss_read_log(paste0(path, "/test4")))

  # qualitative depth
  expect_warning(ss_read_log(paste0(path, "/test5")))

  # unrecognized sensor
  expect_warning(ss_read_log(paste0(path, "/test6")))

  # add area tests when qaqcmar is ready
  # could also add tests for when Hobo/am/vem sensor not found (to increase test coverage)

})
