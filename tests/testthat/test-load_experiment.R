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


test_that("test load_experiment from example dir structure in single process", {
  tmpdir <- withr::local_tempdir()
  fragdir <- build_dir_structure(root = tmpdir)

  example_samplesheet <- system.file(
    "extdata",
    "samplesheet.csv",
    package = "fragmentomics"
  )
  samplesheet <- parse_samplesheet(example_samplesheet)
  results <- load_experiment(samplesheet, fragdir, parallelize = FALSE)
  expect_true(tibble::is_tibble(results))
})

test_that("test load_experiment with non existent root dir", {
  expect_error(
    load_experiment(
      parse_samplesheet(system.file("extdata", "samplesheet.csv",
                                    package = "fragmentomics")),
      root = "non_existent_dir"
    ),
    "Root path does not exist: non_existent_dir"
  )
})

test_that("test load_experiment with skip_matrix_files does not load matrices", {
  tmpdir <- withr::local_tempdir()
  fragdir <- build_dir_structure(root = tmpdir)

  example_samplesheet <- system.file(
    "extdata",
    "samplesheet.csv",
    package = "fragmentomics"
  )
  samplesheet <- parse_samplesheet(example_samplesheet)
  results <- load_experiment(samplesheet, fragdir, skip_matrix_files = TRUE)

  expect_true(tibble::is_tibble(results))
  # expect matrix_file is NA
  expect_true(all(is.na(results$matrixfile)))
})
