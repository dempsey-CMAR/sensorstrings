
test_that("ss_check_station_radius() returns correct result", {

  expect_true(
    ss_check_station_radius(
      log_coords = data.frame(
        station = "Birchy Head",
        latitude = 44.56975, longitude = -64.03448
      ),
      log_crs = 4617,
      station_coords = NULL,
      station_radius = 500
    )
  )

  expect_warning(
    ss_check_station_radius(
      log_coords = data.frame(
        station = "Birchy Head",
        latitude = 45, longitude = -64.03
      ),
      log_crs = 4617,
      station_coords = NULL,
      station_radius = 500
    )
  )

})


# test_that("ss_check_station_in_ocean() returns correct result", {
#
#   expect_true(
#     ss_check_station_in_ocean(
#       log_coords = data.frame(
#         station = "Birchy Head",
#         latitude = 44.56, longitude = -64.03
#       ))
#   )
#
#   expect_warning(
#     ss_check_station_in_ocean(
#       log_coords = data.frame(
#         station = "Birchy Head",
#         latitude = 44.55, longitude = -64.03
#       ))
#   )
#
# })


test_that("ss_check_station_station_drift() returns correct result", {

  expect_true(
    ss_check_station_drift(
      log_coords = data.frame(
        station = "Birchy Head",
        latitude = 44.56, longitude = -64.03,
        retrieval_latitude = 44.56,
        retrieval_longitude = -64.03
      ),
      log_crs = 4617)
  )

  expect_warning(
    ss_check_station_drift(
      log_coords = data.frame(
        station = "Birchy Head",
        latitude = 44.55, longitude = -64.03,
        retrieval_latitude = 44.55,
        retrieval_longitude = -63.96
      ),
      log_crs = 4617)
  )

})
