test_that("selectBins works", {
  test_file <- system.file("extdata", "test_CTCF_compute_matrix.gz", package = "fragmentomics")
  result <- parseComputeMatrix(test_file)
  bins <- selectBins(result)

  # Check if the result is a data frame
  expect_true(tibble::is_tibble(result))

  # Check if the bin columns are renamed correctly
  expect_true("bin_1" %in% colnames(result))
  expect_true("bin_2" %in% colnames(result))
  expect_true("bin_800" %in% colnames(result))

})
