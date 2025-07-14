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
#' @returns A list containing the loaded sample data.
#' @export
load_experiment <- function(samplesheet,
                             rootpath,
                             subdir = "fragmentomics/processed/matrix") {
  samples <- samplesheet |> purrr::pmap(
    function(caseid, sampleid, timepoint, encoded_timepoint) {
      load_matrix_files(caseid,
                        sampleid,
                        timepoint,
                        encoded_timepoint,
                        rootpath,
                        subdir)
    },
    .progress = TRUE
  )
  names(samples) <- samplesheet$sampleid
  cohort <- summarise_cohort(samples, samplesheet)
  targets <- group_targets(samples, samplesheet)

  list(
    samples = samples,
    cohort = cohort,
    targets = targets
  )
}

#'
#' Group targets data
#'
#' @param samples A list of samples matrices loaded from the samplesheet.
#' @param samplesheet A tibble containing the samplesheet data.
#'
group_targets <- function(samples, samplesheet) {
  # group by sources and targets
  source_names <- unique(unlist(lapply(samples, function(sample) {
    names(sample)
  })))

  targets <- lapply(source_names, function(source_name) {
    dplyr::bind_rows(
      lapply(names(samples), function(sample_name) {
        sample <- samples[[sample_name]]
        dplyr::bind_rows(
          lapply(sample[[source_name]], function(target) {
            t <- target$peakstats$average |>
              dplyr::mutate(
                sampleid = sample_name,
                .before = 1
              ) |>
              dplyr::mutate(
                targetid = unique(target$peakstats$stats$target_label),
                .before = 1
              )
            dplyr::left_join(t, samplesheet, by = dplyr::join_by("sampleid")) |>
              dplyr::relocate(dplyr::any_of(names(samplesheet)), .before = 1)
          })
        )
      })
    )
  })
  names(targets) <- source_names
  targets
}

#'
#' Summarise cohort data
#' This function is intended to summarise cohort data.
#'
#' @param samples A list of samples matrices loaded from the samplesheet.
#' @param samplesheet A tibble containing the samplesheet data.
#'
summarise_cohort <- function(samples, samplesheet) {
  # summarise cohort
  cohort <- dplyr::bind_rows(lapply(samples, function(sample) {
    dplyr::bind_rows(lapply(sample, function(source) {
      dplyr::bind_rows(lapply(source, function(target) {
        target$peakstats$stats
      }))
    }))
  }))

  # cohort
  jb <- dplyr::join_by("signal_label" == "sampleid")
  cohort <- dplyr::left_join(cohort, samplesheet, by = jb) |>
    dplyr::mutate(sampleid = signal_label) |>
    dplyr::relocate(dplyr::any_of(names(samplesheet)), .before = 1)

  cohort
}

#'
#' Load matrix files from a directory
#'
#' This function is intended to load matrix files from a specified directory.
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
load_matrix_files <- function(
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

  sample_matrices <- lapply(sources, function(sourcedir) {
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
        matrix = m,
        peakstats = s
      )
    })
    mnames <- tools::file_path_sans_ext(base::basename(matrix_files))
    names(all) <- mnames
    all
  })
  names(sample_matrices) <- basename(sources)
  sample_matrices
}
