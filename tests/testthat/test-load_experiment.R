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
  expect_true(is.list(results$samples))
  s1 <- results$samples[[1]]
  expect_true(is.list(s1))
  source1 <- s1[[1]]
  expect_true(is.list(source1))
  r1 <- source1[[1]]
  expect_true(is.list(r1))
  expect_true(is.data.frame(r1$matrix))
  expect_true(is.list(r1$peakstats))
})
