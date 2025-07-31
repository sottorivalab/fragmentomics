test_that("housekeeping_zscore works", {
  tmpdir <- withr::local_tempdir()
  fragdir <- build_dir_structure(root = tmpdir)

  example_samplesheet <- system.file(
    "extdata",
    "samplesheet.csv",
    package = "fragmentomics"
  )
  samplesheet <- parse_samplesheet(example_samplesheet)
  experiment <- load_experiment(samplesheet, fragdir)

  housekeeping <- experiment |>
    dplyr::filter(target_label == "GeneHancer_housekeeping")

  random <- experiment |>
    dplyr::filter(source_label == "random_dataset")

  zscore_result <- housekeeping_zscore(housekeeping, random)

  expect_true(tibble::is_tibble(zscore_result))
  expect_true("z" %in% colnames(zscore_result))
  expect_true("p" %in% colnames(zscore_result))
})
