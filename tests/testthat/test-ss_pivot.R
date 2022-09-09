
test_that("pivot_* functions return original data frame", {
  expect_equal(depl_all, wide_all)
  expect_equal(long_all, long_all2)

  expect_equal(depl_trim, wide_trim)
  expect_equal(long_trim, long_trim2)
})
