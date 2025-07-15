#'
#' Load data files from a directory
#'
#' This function is intended to load data files from a specified directory.
#'
#' @param caseid A character string specifying the case ID.
#' @param sampleid A character string specifying the sample ID.
#' @param timepoint A factor specifying the timepoint.
#' @param encoded_timepoint An integer specifying the encoded timepoint.
#' @param rootpath A character string specifying the root path where the sample
#' data is located.
#' @param subdir A character string specifying the subdirectory
#' within the root path where
#' the sample data is located, defaults to "fragmentomics/processed/matrix".
#' @export
load_data_files <- function(
    caseid,
    sampleid,
    timepoint,
    encoded_timepoint,
    rootpath,
    subdir) {
  # sample
  sample_root <- file.path(rootpath, caseid, sampleid, subdir)

  # sources
  sources <- list.dirs(sample_root, recursive = FALSE)

  samples_data <- lapply(sources, function(sourcedir) {
    # find matrices in subdirs
    matrix_files <- list.files(
      sourcedir,
      pattern = "\\.gz$",
      full.names = TRUE,
      recursive = TRUE
    )
    all <- lapply(matrix_files, function(mf) {
      m <- parse_compute_matrix(mf)
      sd <- basename(sourcedir)
      s <- peak_stats(m, signal_label = sampleid, source_label = sd)
      list(
        matrix = mf, # just keep a reference to the matrix file location
        peakstats = s
      )
    })
    mnames <- tools::file_path_sans_ext(base::basename(matrix_files))
    names(all) <- mnames
    all
  })
  names(samples_data) <- basename(sources)
  samples_data
}
