
# extract_file_extension()

test_that("extract_file_extension() identifies correct extension", {
  expect_equal(extract_file_extension("fake_file.csv"), "csv")
  expect_equal(extract_file_extension("folder/fake_file.xlsx"), "xlsx")
})
