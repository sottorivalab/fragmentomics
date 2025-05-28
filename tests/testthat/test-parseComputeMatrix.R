test_that("parseComputeMatrix from example file", {
  test_file <- system.file("extdata", "test_CTCF_compute_matrix.gz", package = "fragmentomics")
  result <- parseComputeMatrix(test_file)

  # Check if the result is a data frame
  expect_true(tibble::is_tibble(result))

  # Check if the expected columns are present
  expect_true(all(c("chr", "start", "end", "name", "score", "strand") %in% colnames(result)))

  # Check if the bin columns are renamed correctly
  expect_true("bin_1" %in% colnames(result))
  expect_true("bin_2" %in% colnames(result))
  expect_true("bin_800" %in% colnames(result))

  # Check if NaN values are replaced with NA
  expect_true(all(is.na(result$bin_1[is.nan(result$bin_1)])))

  # Check if the data frame has the expected number of rows
  expect_equal(nrow(result), 9950)  # Adjust this number based on the expected number of rows in the test file
  expect_equal(ncol(result), 23 + 6 + 800)  # 23 meta columns + 6 fixed columns + 800 bin columns
})
