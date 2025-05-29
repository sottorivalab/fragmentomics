test_that("peakStats works", {
  test_file <- system.file("extdata", "CTCF.gz", package = "fragmentomics")
  data <- parseComputeMatrix(test_file)

  # Test with default parameters
  result <- peakStats(data)
  
  # Check if the result is a data frame
  expect_true(is.list(result))
  
  # Check if the expected columns are present
  expect_true(all(c("name", "bin", "coverage", "relative", "mean") %in% colnames(result[['average']])))

})
