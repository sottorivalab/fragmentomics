test_that("parse_compute_matrix from example file", {
  test_file <- system.file("extdata", "CTCF_matrix.gz", package = "fragmentomics")
  matrix_data <- parse_compute_matrix(test_file)
  h1 <- matrix_plot(matrix_data)

  expect_true(is(h1, "Heatmap"))  # Check if h1 is a ggplot object
})
