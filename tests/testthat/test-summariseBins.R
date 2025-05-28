test_that("summarise bins", {
    # Load the example data
    test_file <- system.file("extdata", "test_CTCF_compute_matrix.gz", package = "fragmentomics")
    result <- parseComputeMatrix(test_file)
    # Select the bin columns
    bin_data <- selectBins(result)
    # Summarise the bins
    summarised_data <- summariseBins(bin_data)
    # Check if the result is a data frame
    expect_true(tibble::is_tibble(summarised_data))
    # Check if the bin columns are summarised correctly
    expect_true("bin_1" %in% colnames(summarised_data))
    expect_true("bin_2" %in% colnames(summarised_data))
    expect_true("bin_800" %in% colnames(summarised_data))
    # Check if the data frame has the expected number of rows
    expect_equal(nrow(summarised_data), 1)  
    # Adjust this number based on the expected number of rows in the test file
    expect_equal(ncol(summarised_data), 800)  # 23 meta columns + 6 fixed columns + 800 bin columns
})
