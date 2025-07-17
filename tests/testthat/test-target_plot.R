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
  ctcf <- load_peaks(experiment |> dplyr::filter(target_label == "CTCF"), fragdir)

  g <- target_plot(ctcf)
  expect_true(ggplot2::is_ggplot(g))
})
