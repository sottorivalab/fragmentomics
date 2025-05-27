test_that("parseComputeMatrix works", {
  # Call the function with mock data and headers
  test_file <- system.file("extdata", "test_CTCF_compute_matrix.gz", package = "fragmentomics")
  result <- parseComputeMatrix(test_file)

  # Check if the result is a data frame
  expect_true(tibble::is_tibble(result))

  # Check if the expected columns are present
  expect_true(all(c("chr", "start", "end", "name", "score", "strand") %in% colnames(result)))

  # Check if the bin columns are renamed correctly
  expect_true("bin_1" %in% colnames(result))
  expect_true("bin_2" %in% colnames(result))

  # Check if NaN values are replaced with NA
  expect_true(all(is.na(result$bin_1[is.nan(result$bin_1)])))
})
