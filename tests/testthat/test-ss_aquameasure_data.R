
# ss_read_aquameasure_data -------------------------------------------------------

test_that("ss_read_aquameasure_data() reads in correct classes", {
  expect_equal(class(am1$`Timestamp(UTC)`), "character")
  expect_equal(class(am1$`Time Corrected(seconds)`), "integer")
  expect_equal(class(am1$Sensor), "character")
  expect_equal(class(am1$`Record Type`), "character")
  expect_equal(class(am1$`Dissolved Oxygen`), "numeric")
  expect_equal(class(am1$Temperature), "numeric")
  expect_equal(class(am1$`Device Tilt`), "numeric")
  expect_equal(class(am1$`Battery Voltage`), "numeric")
})

test_that("ss_read_aquameasure_data() reads in all observations", {
  expect_equal(nrow(am1), 47)
})
