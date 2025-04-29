# extract_file_extension()

test_that("extract_file_extension() identifies correct extension", {
  expect_equal(extract_file_extension("fake_file.csv"), "csv")
  expect_equal(extract_file_extension("folder/fake_file.xlsx"), "xlsx")
  expect_warning(extract_file_extension("folder/fake_file"))
  expect_error(extract_file_extension(NULL))
})

# ss_coords_from_ddm_to_dd()
test_that("ss_coords_from_ddm_to_dd() returns correct coordinates", {
  expect_equal(
    round(ss_coords_from_ddm_to_dd(coords_ddm), digits = 5),
    c(45.36085, 61.40678, 44.43730, 64.25063)
  )
})
