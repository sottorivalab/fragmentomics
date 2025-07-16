test_that("parse_peak_data works", {
  test_file <- system.file("extdata",
                           "CTCF_peak_data.tsv",
                           package = "fragmentomics")
  # Parse the peak data
  peak_data <- parse_peak_data(test_file)
  # Check if the result is a data frame
  expect_true(is.data.frame(peak_data))
  # Check if the expected columns are present
  expect_true(
    all(
      c("bin",
        "coverage",
        "relative",
        "background.mean"
      ) %in% colnames(peak_data)
    )
  )
})
