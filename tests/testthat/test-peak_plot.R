test_that("peakPlot works", {
  test_file <- system.file("extdata", "CTCF.gz", package = "fragmentomics")
  # Compute peak stats
  stats <- peak_stats(parse_compute_matrix(test_file))
  # Test peak plot function
  plot <- peak_plot(stats)
  # Check if the plot is a ggplot object
  expect_true(ggplot2::is.ggplot(plot))
})
