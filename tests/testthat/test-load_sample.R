test_that("test load_sample from example dir structure", {
  tmpdir <- withr::local_tempdir()
  fragdir <- build_dir_structure(root = tmpdir)

  example_samplesheet <- system.file(
    "extdata",
    "samplesheet.csv",
    package = "fragmentomics"
  )
  samplesheet <- fragmentomics::parse_samplesheet(example_samplesheet)

  results <- fragmentomics::load_sample(
    caseid = samplesheet$caseid[1],
    sampleid = samplesheet$sampleid[1],
    timepoint = samplesheet$timepoint[1],
    encoded_timepoint = samplesheet$encoded_timepoint[1],
    rootpath = fragdir,
    subdir = "fragmentomics/processed"
  )

  expect_true(tibble::is_tibble(results))
})

test_that("test load_samples fail with missing peak stats file", {
  tmpdir <- withr::local_tempdir()
  fragdir <- build_dir_structure(root = tmpdir)

  example_samplesheet <- system.file(
    "extdata",
    "samplesheet.csv",
    package = "fragmentomics"
  )

  samplesheet <- fragmentomics::parse_samplesheet(example_samplesheet)
  f <- "SAMPLE_1/SAMPLE_1_BL/fragmentomics/processed/peakstats/source_1/CTCF"
  file_to_remove <- file.path(fragdir, f, "CTCF_peak_stats.tsv")
  file.remove(file_to_remove)
  expect_error(
    fragmentomics::load_sample(
      caseid = samplesheet$caseid[1],
      sampleid = samplesheet$sampleid[1],
      timepoint = samplesheet$timepoint[1],
      encoded_timepoint = samplesheet$encoded_timepoint[1],
      rootpath = fragdir,
      subdir = "fragmentomics/processed"
    ),
    "peak_stats_file path does not exist"
  )
})

test_that("test load_samples fail with missing peak data file", {
  tmpdir <- withr::local_tempdir()
  fragdir <- build_dir_structure(root = tmpdir)

  example_samplesheet <- system.file(
    "extdata",
    "samplesheet.csv",
    package = "fragmentomics"
  )

  samplesheet <- fragmentomics::parse_samplesheet(example_samplesheet)
  f <- "SAMPLE_1/SAMPLE_1_BL/fragmentomics/processed/peakstats/source_1/CTCF"
  file_to_remove <- file.path(fragdir, f, "CTCF_peak_data.tsv")
  file.remove(file_to_remove)
  expect_error(
    fragmentomics::load_sample(
      caseid = samplesheet$caseid[1],
      sampleid = samplesheet$sampleid[1],
      timepoint = samplesheet$timepoint[1],
      encoded_timepoint = samplesheet$encoded_timepoint[1],
      rootpath = fragdir,
      subdir = "fragmentomics/processed"
    ),
    "peak_data_file path does not exist"
  )
})

test_that("test load_samples fail with missing matrix file", {
  tmpdir <- withr::local_tempdir()
  fragdir <- build_dir_structure(root = tmpdir)

  example_samplesheet <- system.file(
    "extdata",
    "samplesheet.csv",
    package = "fragmentomics"
  )

  samplesheet <- fragmentomics::parse_samplesheet(example_samplesheet)
  f <- "SAMPLE_1/SAMPLE_1_BL/fragmentomics/processed/matrix/source_1/CTCF"
  file_to_remove <- file.path(fragdir, f, "CTCF.gz")
  file.remove(file_to_remove)

  expect_error(
    fragmentomics::load_sample(
      caseid = samplesheet$caseid[1],
      sampleid = samplesheet$sampleid[1],
      timepoint = samplesheet$timepoint[1],
      encoded_timepoint = samplesheet$encoded_timepoint[1],
      rootpath = fragdir,
      subdir = "fragmentomics/processed"
    ),
    "matrix_file_name path does not exist"
  )
})
