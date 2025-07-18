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
  housekeeping <- load_peaks(experiment |> dplyr::filter(target_label == "GeneHancer_housekeeping"),
                     fragdir)
  random <- load_peaks(experiment |> dplyr::filter(source_label == "random_dataset"),
                       fragdir)
  g <- housekeeping_plot(housekeeping, random)
  expect_true(ggplot2::is_ggplot(g))
})
