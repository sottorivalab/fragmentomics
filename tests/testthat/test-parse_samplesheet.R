test_that("parse_samplesheet works", {
  test_file <- system.file(
    "extdata",
    "samplesheet.csv",
    package = "fragmentomics"
  )
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
        "encoded_timepoint"
      ) %in% colnames(samplesheet)
    )
  )
})

test_that("parse_samplesheet error with non existent file", {
  expect_error(
    parse_samplesheet("non_existent_file.txt"),
    "File does not exist: non_existent_file.txt"
  )
})

test_that("parse_samplesheet error with non existent timepoints", {
  test_file <- system.file(
    "extdata",
    "samplesheet.csv",
    package = "fragmentomics"
  )
  expect_error(
    parse_samplesheet(test_file, timepoint_levels = c("XR", "XX")),
    "Not all timepoints in samplesheet are in the expected levels: XR, XX"
  )
})

