test_that("parseComputeMatrix from example file", {
  test_file <- system.file("extdata", "CTCF.gz", package = "fragmentomics")
  result <- parse_compute_matrix(test_file)
  # Check if the result is a data frame
  expect_true(tibble::is_tibble(result))
  # Check if the expected columns are present
  expect_true(
    all(
      c(
        "chr",
        "start",
        "end",
        "name",
        "score",
        "strand",
        "file",
        "upstream",
        "downstream",
        "body",
        "bin_size",
        "ref_point") %in% colnames(result)
    )
  )
  # Check if the bin columns are renamed correctly
  expect_true("bin_1" %in% colnames(result))
  expect_true("bin_2" %in% colnames(result))
  expect_true("bin_800" %in% colnames(result))
  # Check if NaN values are replaced with NA
  expect_true(all(is.na(result$bin_1[is.nan(result$bin_1)])))
  # Check if the data frame has the expected number of rows
  expect_equal(nrow(result), 1000)
  expect_equal(ncol(result), 23 + 6 + 800)
})

test_that("parseComputeMatrix with non existent file", {
  expect_error(
    parse_compute_matrix("non_existent_file.txt"),
    "File does not exist: non_existent_file.txt"
  )
})

test_that("parseComputeMatrix with empty file", {
  empty_file <- system.file("extdata", "empty.gz", package = "fragmentomics")
  expect_error(
    parse_compute_matrix(empty_file),
    "no lines available in input"
  )
})

test_that("parseComputeMatrix with insufficient columns", {
  empty_file <- system.file("extdata", "nocols.gz", package = "fragmentomics")
  expect_error(
    parse_compute_matrix(empty_file),
    "The file does not contain enough columns!"
  )
})

