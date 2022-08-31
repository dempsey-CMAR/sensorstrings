
test_that("pivot_* functions return original data frame", {
  expect_equal(depl_trim, dat_wide)
  expect_equal(dat_long, dat_long2)
})
