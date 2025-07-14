test_that("target_plot return a ggplot", {
  tmpdir <- withr::local_tempdir()
  fragdir <- build_dir_structure(root = tmpdir)

  example_samplesheet <- system.file(
    "extdata",
    "samplesheet.csv",
    package = "fragmentomics"
  )
  samplesheet <- parse_samplesheet(example_samplesheet)
  experiment <- load_experiment(samplesheet, fragdir)
  target_plot <- target_plot(experiment, "CTCF", "source_1")
  expect_true(ggplot2::is_ggplot(target_plot))
})
