
test_that("dissolved oxygen correction factors are correct", {
  # compare to USGS table
  expect_equal(usgs, F_bk)

  # compare to sensorstrings 0.1.0
  expect_equal(F_0, F_gg$`0`)
  expect_equal(F_15, F_gg$`15`)
  expect_equal(F_30, F_gg$`30`)

  expect_equal(F_p05, F_p$`0.5`)
  expect_equal(F_p11, F_p$`1.1`)
})


test_that("dissolved oxygen correction functions return expected errors", {

  expect_error(ss_dissolved_oxygen_salinity_correction(temp_sal, sal = 0))
  expect_error(ss_dissolved_oxygen_salinity_correction(temp_sal, method = "spelling error"))

  expect_error(ss_dissolved_oxygen_pressure_correction(temp_press, p_atm = 0.5))
  expect_error(ss_dissolved_oxygen_pressure_correction(temp_sal, method = "spelling error"))

})
