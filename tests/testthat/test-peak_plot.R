test_that("peakPlot works", {
  test_file <- system.file("extdata", "CTCF.gz", package = "fragmentomics")
  # Compute peak stats
  stats <- peak_stats(parse_compute_matrix(test_file))
  # Test peak plot function
  plot <- peak_plot(stats)
  # Check if the plot is a ggplot object
  expect_true(ggplot2::is_ggplot(plot))
  # Check if the plot has the expected title
  expect_equal(
   plot$labels$title,
    "Composite coverage: CTCF on TEST_SAMPLE"
  )
})

test_that("peakPlot normalized works", {
  test_file <- system.file("extdata", "CTCF.gz", package = "fragmentomics")
  # Compute peak stats
  stats <- peak_stats(parse_compute_matrix(test_file))
  # Test peak plot function
  plot <- peak_plot(stats, normalized=T)
  # Check if the plot is a ggplot object
  expect_true(ggplot2::is_ggplot(plot))
})
