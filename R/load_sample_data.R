#'
#' Load sample data from a samplesheet
#' This function is intended to load sample data based on a provided samplesheet.
#'
#' @param samplesheet A character string specifying the path to the samplesheet file.
#' @param rootpath A character string specifying the root path where the sample data is located.
#' @param subdir A character string specifying the subdirectory within the root path where
#' the sample data is located, defaults to "fragmentomics/processed/matrix".
#' @returns A list containing the loaded sample data.
#' @export
load_sample_data <- function(samplesheet, rootpath, subdir = "fragmentomics/processed/matrix") {
  samples <- samplesheet |> purrr::pmap(function(caseid, sampleid, timepoint, encoded_timepoint) {

    # sample
    sample_root <- file.path(rootpath, caseid, sampleid, subdir)

    # sources
    sources <- list.dirs(sample_root, recursive = FALSE)

    s <- lapply(sources, function(sourcedir){
      # find matrices in subdirs
      matrix_files <- list.files(
        sourcedir,
        pattern = "\\.gz$",
        full.names = TRUE,
        recursive = TRUE
      )
      all <- lapply(matrix_files, function(mf){
        m <- parse_compute_matrix(mf)
        s <- peak_stats(m)
        list(
          matrix = m,
          stats = s
        )
      })
      mnames <- tools::file_path_sans_ext(base::basename(matrix_files))
      names(all) <- mnames
      all
    })
    names(s) <- basename(sources)
    s
  })
  names(samples) <- samplesheet$sampleid
  samples
}
