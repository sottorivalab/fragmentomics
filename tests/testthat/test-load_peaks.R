test_that("test load_peaks from example dir structure", {
  tmpdir <- withr::local_tempdir()
  fragdir <- build_dir_structure(root = tmpdir)

  example_samplesheet <- system.file(
    "extdata",
    "samplesheet.csv",
    package = "fragmentomics"
  )
  samplesheet <- parse_samplesheet(example_samplesheet)
  experiment <- load_experiment(samplesheet, fragdir)
  ctcf <- experiment |> dplyr::filter(target_label == "CTCF")
  ctcf_peaks <- load_peaks(ctcf, fragdir)
  expect_true(tibble::is_tibble(ctcf_peaks))
})

test_that("test load_peaks from missing dir structure", {
  tmpdir <- withr::local_tempdir()
  fragdir <- build_dir_structure(root = tmpdir)

  example_samplesheet <- system.file(
    "extdata",
    "samplesheet.csv",
    package = "fragmentomics"
  )
  samplesheet <- parse_samplesheet(example_samplesheet)
  experiment <- load_experiment(samplesheet, fragdir)

  expect_error(
    load_peaks(experiment |> dplyr::filter(target_label == "CTCF"),
               "non_existent_dir"),
    "Root path does not exist: non_existent_dir"
  )
})

test_that("test load_peaks from example dir structure single process", {
  tmpdir <- withr::local_tempdir()
  fragdir <- build_dir_structure(root = tmpdir)

  example_samplesheet <- system.file(
    "extdata",
    "samplesheet.csv",
    package = "fragmentomics"
  )
  samplesheet <- parse_samplesheet(example_samplesheet)
  experiment <- load_experiment(samplesheet, fragdir, parallelize = FALSE)
  ctcf <- experiment |> dplyr::filter(target_label == "CTCF")
  ctcf_peaks <- load_peaks(ctcf, fragdir)
  expect_true(tibble::is_tibble(ctcf_peaks))
})
