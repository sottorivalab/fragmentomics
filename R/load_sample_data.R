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
        s <- peak_stats(m, signal_label = sampleid, source_label = basename(sourcedir))
        list(
          matrix = m,
          peakstats = s
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

  # summarise cohort
  cohort <- dplyr::bind_rows(lapply(samples, function(sample) {
    dplyr::bind_rows(lapply(sample, function(source) {
      dplyr::bind_rows(lapply(source, function(target) {
        target$peakstats$stats
      }))
    }))
  }))

  # cohort
  cohort <- dplyr::left_join(cohort, samplesheet, by=dplyr::join_by("signal_label" == "sampleid")) |>
    dplyr::mutate(
      sampleid = signal_label
    ) |>
    dplyr::relocate(dplyr::any_of(names(samplesheet)), .before = 1)

  # group by sources and targets
  source_names <- unique(unlist(lapply(samples, function(sample){
    names(sample)
  })))

  targets <- lapply(source_names, function(source_name) {
    dplyr::bind_rows(
      lapply(names(samples), function(sample_name){
        sample <- samples[[sample_name]]
        dplyr::bind_rows(
          lapply(sample[[source_name]], function(target){
            target$peakstats$average |>
              dplyr::mutate(
                sampleid = sample_name,
                .before = 1
              ) |>
              dplyr::mutate(
                targetid = unique(target$peakstats$stats$target_label),
                .before = 1
              )
          })
        )
      })
    )
  })
  names(targets) <- source_names


  list(
    samples = samples,
    cohort = cohort,
    targets = targets
  )
}
