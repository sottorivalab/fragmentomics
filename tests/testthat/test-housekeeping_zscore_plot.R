test_that("housekeeping_zscore_plot works", {
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

  g1 <- housekeeping_zscore_plot(housekeeping, random)
  expect_true(ggplot2::is_ggplot(g1))
})
