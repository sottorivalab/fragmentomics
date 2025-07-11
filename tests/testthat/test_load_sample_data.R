test_that("test load_sample_data from example dir structure", {
  tmpdir <- withr::local_tempdir()
  fragdir <- build_dir_structure(root = tmpdir)

  example_samplesheet <- system.file(
    "extdata",
    "samplesheet.csv",
    package = "fragmentomics"
  )
  samplesheet <- parse_samplesheet(example_samplesheet)
  results <- load_sample_data(samplesheet, fragdir)
  expect_true(is.list(results))
  s1 <- results[[1]]
  expect_true(is.list(s1))
  source1 <- s1[[1]]
  expect_true(is.list(source1))
  m1 <- source1[[1]]
  expect_true(is.data.frame(m1))
})
