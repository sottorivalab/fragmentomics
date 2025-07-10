#'
#' Load sample data from a samplesheet
#' This function is intended to load sample data based on a provided samplesheet.
#'
#' @param samplesheet A character string specifying the path to the samplesheet file.
#' @param rootpath A character string specifying the root path where the sample data is located.
#' @param subdir A character string specifying the subdirectory within the root path where
#' the sample data is located, defaults to "fragmentomics/processed/matrix".
load_sample_data <- function(samplesheet, rootpath, subdir = "fragmentomics/processed/matrix") {
  samplesheet |> purrr::pmap(function(caseid, sampleid, timepoint, encoded_timepoint) {
    sample_root <- file.path(rootpath, caseid, sampleid, subdir)
    print(sample_root)
  })
}
