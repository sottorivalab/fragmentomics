test_that("peakPlot works", {
  test_file <- system.file("extdata", "test_CTCF_compute_matrix.gz", package = "fragmentomics")
  
  # Compute peak stats
  stats <- peakStats(parseComputeMatrix(test_file))
    
  # Test peakPlot function
  plot <- peakPlot(stats)
  
  # Check if the plot is a ggplot object
  expect_true(ggplot2::is.ggplot(plot))
})
