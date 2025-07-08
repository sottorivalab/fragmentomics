test_that("parse_samplesheet works", {
  test_file <- system.file("extdata", "samplesheet.csv", package = "fragmentomics")
  # Parse the samplesheet
  samplesheet <- parse_samplesheet(test_file)
  # Check if the result is a data frame
  expect_true(is.data.frame(samplesheet))
  # Check if the expected columns are present
  expect_true(
    all(
      c(
        "caseid",
        "sampleid",
        "timepoint",
        "bam",
        "bai",
        "bw"
      ) %in% colnames(samplesheet)
    )
  )
})
