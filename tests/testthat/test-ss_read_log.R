test_that("ss_read_log() reads in all observations", {
  expect_equal(nrow(log_old), 7)
  expect_equal(ncol(log_old), 14)
})

test_that("ss_read_log() reads data when path includes file name", {
  expect_equal(log_old, log_old2)
})


# test_that("ss_read_log() reads in old columns", {
#   expect_true(
#     all(colnames(log_old) %in%
#           c("County",
#             "Deployment_Waterbody",
#             "Location_Description",
#             "Lease#", "Status", "Deployment", "Retrieval",
#             "Duration", "Logger_Latitude", "Logger_Longitude",
#             "Logger_Model", "Serial#", "Sensor_Depth", "Configuration")))
# })

test_that("ss_read_log() reads in new columns", {
  expect_true(
    all(colnames(log_new) %in%
          c("station", "waterbody", "lease",
            "status", "deployment_date", "deployment_time_utc",
            "retrieval_date", "retrieval_time_utc", "deployment_latitude",
            "deployment_longitude", "sensor_type", "sensor_serial_number",
            "sensor_depth_m", "string_configuration")))
})


# ss_parse_log() ----------------------------------------------------------

test_that("ss_parse_log() returns correct dimensions", {

  expect_equal(length(ss_parse_log(log_new, verbose = FALSE)), 4)
  expect_equal(
    length(
      Filter(Negate(is.null),
             ss_parse_log(
               log_new, verbose = FALSE,
               deployment_dates = FALSE, area_info = FALSE, config = FALSE))),
    1)
})

test_that("ss_parse_log() returns Errors and Warnings", {
  # 2 files in log folder
  expect_error(ss_read_log(paste0(path, "/test1")))

  # multiple deployment dates
  expect_warning(ss_read_log(paste0(path, "/test2"), verbose = FALSE))

  # multiple retrieval dates
  expect_warning(ss_read_log(paste0(path, "/test3"), verbose = FALSE))

  # deployment dates are in wrong order
  expect_warning(ss_read_log(paste0(path, "/test4"), verbose = FALSE))

  # qualitative depth
  expect_warning(ss_read_log(paste0(path, "/test5"), verbose = FALSE))

  # unrecognized sensor
  expect_warning(ss_read_log(paste0(path, "/test6"), verbose = FALSE))

})
