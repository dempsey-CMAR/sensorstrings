
# ss_read_vemco_data -------------------------------------------------------

test_that("ss_read_vemco_data() reads in correct classes", {
  expect_equal(class(vemco1$`Date and Time (UTC)`), "character")
  expect_equal(class(vemco1$Receiver), "character")

  expect_equal(class(vemco1$Description), "character")
  expect_equal(class(vemco1$Data), "character")
  expect_equal(class(vemco1$Units), "character")
})


test_that("ss_read_vemco_data() reads in all observations", {
  expect_equal(nrow(vemco1), 124)
})


test_that("ss_read_vemco_data() report error for excel files", {
  expect_error(ss_read_vemco_data(path, "error.xls"))
  expect_error(ss_read_vemco_data(path, "error.xlsx"))
})
