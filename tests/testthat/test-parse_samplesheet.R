test_that("parse_samplesheet works", {
  test_file <- system.file(
    "extdata",
    "samplesheet.csv",
    package = "fragmentomics"
  )
  # Parse the samplesheet
  samplesheet <- parse_samplesheet(test_file, "results")
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
    parse_samplesheet("non_existent_file.txt","results"),
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
    parse_samplesheet(test_file, "results", timepoint_levels = c("XR", "XX")),
    "Not all timepoints in samplesheet are in the expected levels: XR, XX"
  )
})

test_that("parse_samplesheet produces correct datapaths", {
  test_file <- system.file(
    "extdata",
    "samplesheet.csv",
    package = "fragmentomics"
  )
  samplesheet <- parse_samplesheet(test_file, "results")

  # Check if the datapath column is correctly constructed
  expected_datapaths <- c(
    "results/SAMPLE_1/SAMPLE_1_BL/fragmentomics/processed/matrix",
    "results/SAMPLE_1/SAMPLE_1_BR/fragmentomics/processed/matrix",
    "results/SAMPLE_2/SAMPLE_2_BR/fragmentomics/processed/matrix",
    "results/SAMPLE_2/SAMPLE_2_BL/fragmentomics/processed/matrix",
    "results/SAMPLE_3/SAMPLE_3_BL/fragmentomics/processed/matrix",
    "results/SAMPLE_3/SAMPLE_3_PD/fragmentomics/processed/matrix"
  )
  expect_equal(samplesheet$datapath, expected_datapaths)
})

