test_that("test load_experiment from example dir structure", {
  tmpdir <- withr::local_tempdir()
  fragdir <- build_dir_structure(root = tmpdir)

  example_samplesheet <- system.file(
    "extdata",
    "samplesheet.csv",
    package = "fragmentomics"
  )
  samplesheet <- parse_samplesheet(example_samplesheet)
  results <- load_experiment(samplesheet, fragdir)
  expect_true(tibble::is_tibble(results))
})
