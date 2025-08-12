test_that("cohort_matrixes works", {
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
  expect_true(is.list(matrixes))
  expect_true(all(sapply(matrixes, is.matrix)))
  expect_true(all(sapply(matrixes, function(m) nrow(m) > 0)))
})
