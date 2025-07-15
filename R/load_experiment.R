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
#' @param number_of_daemons An integer specifying the number of daemons to use
#'
#' @returns A tibble
#'
#' @export
load_experiment <- function(samplesheet,
                            rootpath,
                            subdir = "fragmentomics/processed",
                            parallelize = TRUE,
                            number_of_daemons = parallel::detectCores()) {

  if (!file.exists(rootpath)) {
    stop("Root path does not exist: ", rootpath)
  }

  if (parallelize) {
    # FIXME can be also slurm
    mirai::daemons(number_of_daemons)
    samples <- samplesheet |> purrr::pmap(
      purrr::in_parallel(
        \(caseid, sampleid, timepoint, encoded_timepoint) {
            fragmentomics::load_data_files(caseid,
                                           sampleid,
                                           timepoint,
                                           encoded_timepoint,
                                           rootpath,
                                           subdir)
        },
        rootpath = rootpath,
        subdir = subdir
      )
    ) |> dplyr::bind_rows()
    mirai::daemons(0)
    samples
  } else {
    samplesheet |> purrr::pmap(
      function(caseid, sampleid, timepoint, encoded_timepoint) {
        fragmentomics::load_data_files(caseid,
                                       sampleid,
                                       timepoint,
                                       encoded_timepoint,
                                       rootpath,
                                       subdir)
      },
      .progress = TRUE
    ) |> dplyr::bind_rows()
  }
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

