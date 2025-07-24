test_that("housekeeping_plot return a ggplot", {
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
  housekeeping_data <- load_peaks(housekeeping, fragdir)

  random <- experiment |>
    dplyr::filter(source_label == "random_dataset")
  random_data <- load_peaks(random, fragdir)

  g <- housekeeping_plot(housekeeping_data, random_data)
  expect_true(ggplot2::is_ggplot(g))
})
