test_that("peakStats works", {
  test_file <- system.file("extdata",
                           "CTCF_matrix.gz",
                           package = "fragmentomics")
  data <- parse_compute_matrix(test_file)
  # Test with default parameters
  result <- peak_stats(data)
  # Check if the result is a data frame
  expect_true(is.list(result))
  # Check if the expected columns are present
  expect_true(
    all(
      c(
        "name",
        "bin",
        "coverage",
        "relative",
        "mean"
      ) %in% colnames(result[["average"]])
    )
  )
  expect_true(
    all(
      c(
        "target_label",
        "signal_label",
        "source_label",
        "bin_size",
        "central_bin",
        "peak_length",
        "reference_point_coverage",
        "central_coverage",
        "average_coverage",
        "background_mean"
      ) %in% names(result[["stats"]])
    )
  )
})
