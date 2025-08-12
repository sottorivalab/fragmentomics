test_that("cohort_heatmap works", {
  tmpdir <- withr::local_tempdir()
  fragdir <- build_dir_structure(root = tmpdir)

  example_samplesheet <- system.file(
    "extdata",
    "samplesheet.csv",
    package = "fragmentomics"
  )
  samplesheet <- parse_samplesheet(example_samplesheet)
  experiment <- load_experiment(samplesheet, fragdir) |>
    dplyr::filter(source_label == "single_region_files")

  # refator signal label
  experiment <- experiment |> dplyr::mutate(signal_label = sampleid)

  # test cohort_matrixes
  matrixes <- cohort_matrixes(experiment)
  h1 <- cohort_heatmap(matrixes)

  expect_true(is(h1, "HeatmapList"))  # Check if h1 is a ggplot object

})
