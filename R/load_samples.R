#'
#' Load data files from a sample directory
#'
#' This function is intended to load data files from a
#' specified sample directory.
#'
#' @param caseid A character string specifying the case ID.
#' @param sampleid A character string specifying the sample ID.
#' @param timepoint A factor specifying the timepoint.
#' @param encoded_timepoint An integer specifying the encoded timepoint.
#' @param rootpath A character string specifying the root path where the sample
#' data is located.
#' @param subdir A character string specifying the subdirectory
#' within the root path where
#' the sample data is located, defaults to "fragmentomics/processed".
#' @export
load_samples <- function(
    caseid,
    sampleid,
    timepoint,
    encoded_timepoint,
    rootpath,
    subdir) {

  # sample root example: "results/SAMPLE_3/SAMPLE_3_PD/fragmentomics/processed"
  sample_root <- file.path(rootpath, caseid, sampleid, subdir)

  # list sources in peak stats dir
  peakstats_dir <- file.path(sample_root, "peakstats")
  sources <- list.dirs(peakstats_dir, recursive = FALSE)

  # iterate in sources
  dplyr::bind_rows(lapply(sources, function(sourcedir) {

    # iterate in targets
    target_dirs <- list.dirs(sourcedir, recursive = FALSE)

    dplyr::bind_rows(
      lapply(target_dirs, function(target_dir) {

        # stats file
        pf_name <- paste(basename(target_dir), "_peak_stats.tsv", sep = "")
        peak_stats_file <- file.path(target_dir, pf_name)
        if (!file.exists(peak_stats_file)) {
          stop("peak_stats_file path does not exist: ", peak_stats_file)
        }

        # data file
        pf_name <- paste(basename(target_dir), "_peak_data.tsv", sep = "")
        peak_data_file <- file.path(target_dir, pf_name)
        if (!file.exists(peak_data_file)) {
          stop("peak_data_file path does not exist: ", peak_data_file)
        }

        # find corresponding matrix file
        matrix_path <- stringr::str_replace(target_dir, "peakstats", "matrix")
        matrix_file_name <- file.path(matrix_path,
                                      paste(basename(target_dir),
                                            ".gz", sep = ""))

        if (!file.exists(matrix_file_name)) {
          matrix_file_name <- file.path(matrix_path,
                                        paste(basename(target_dir),
                                              "_matrix.gz", sep = ""))
        }

        if (!file.exists(matrix_file_name)) {
          stop("matrix_file_name path does not exist: ", matrix_file_name)
        }

        # build tibble
        fragmentomics::parse_peak_stats(peak_stats_file) |>
          dplyr::mutate(
            caseid = caseid,
            sampleid = sampleid,
            timepoint = timepoint,
            encoded_timepoint = encoded_timepoint,
            .before = 1
          ) |>
          dplyr::mutate(
            peakfile = peak_data_file,
            matrixfile = matrix_file_name,
            .after = "source"
          )
      })
    )
  }))

}
