#'
#' Load sample data from a samplesheet
#' This function is intended to load sample data
#' based on a provided samplesheet.
#'
#' @param samplesheet A character string specifying
#' the path to the samplesheet file.
#' @param rootpath A character string specifying the
#' root path where the sample data is located.
#' @param subdir A character string specifying the
#' subdirectory within the root path where
#' the sample data is located, defaults
#' to "fragmentomics/processed/matrix".
#' @param parallelize A logical value indicating whether to parallelize the
#' loading of sample data, defaults to TRUE.#' loading of sample data,
#' defaults to TRUE.
#' @param skip_matrix_files A logical value indicating whether to skip loading
#' matrix files, defaults to FALSE.
#' @param number_of_daemons An integer specifying the number of daemons to use
#'
#' @returns A tibble containing the loaded sample data.
#'
#' @examples
#' \dontrun{
#' example_samplesheet <- system.file("extdata",
#'                                    "samplesheet.csv",
#'                                    package = "fragmentomics")
#' samplesheet <- parse_samplesheet(example_samplesheet)
#' experiment <- load_experiment(samplesheet, "results")
#' }
#' @export
load_experiment <- function(samplesheet,
                            rootpath,
                            subdir = "fragmentomics/processed",
                            parallelize = TRUE,
                            skip_matrix_files = FALSE,
                            number_of_daemons = parallelly::availableCores()) {

  if (!file.exists(rootpath)) {
    stop("Root path does not exist: ", rootpath)
  }

  if (parallelize) {
    # FIXME can be also slurm
    mirai::daemons(number_of_daemons)
    tryCatch(
      samples <- parallel_load_samples(samplesheet, rootpath,
                                       subdir, skip_matrix_files),
      finally = {
        mirai::daemons(0)
      }
    )
    samples
  } else {
    samplesheet |> purrr::pmap(
      function(caseid, sampleid, timepoint, encoded_timepoint) {
        fragmentomics::load_sample(caseid, sampleid, timepoint,
                                   encoded_timepoint, rootpath,
                                   subdir, skip_matrix_files)
      }
    ) |>
      dplyr::bind_rows()
  }
}


#'
#' Load samples in parallel
#' This function is intended to load samples in parallel
#'
#' @param samplesheet A tibble containing the samplesheet data.
#' @param rootpath A character string specifying the
#' root path where the sample data is located.
#' @param subdir A character string specifying the
#' @param skip_matrix_files A logical value indicating whether to skip loading
#' subdirectory within the root path where
#' the sample data is located, defaults
#' to "fragmentomics/processed/matrix".
#'
#' @returns A tibble containing the loaded sample data.
parallel_load_samples <- function(samplesheet,
                                  rootpath,
                                  subdir,
                                  skip_matrix_files) {
  samplesheet |> purrr::pmap(
    purrr::in_parallel(
      \(caseid, sampleid, timepoint, encoded_timepoint) {
        fragmentomics::load_sample(caseid, sampleid, timepoint,
                                   encoded_timepoint, rootpath,
                                   subdir, skip_matrix_files)
      },
      rootpath = rootpath,
      subdir = subdir,
      skip_matrix_files = skip_matrix_files
    )
  ) |>
    dplyr::bind_rows()
}
