#' Load peaks
#' take in input an experiment data structure and return
#' a peaks data structure loading peaks data files
#'
#' @param experiment A tibble containing the experiment data.
#' @param rootpath A character string specifying the
#' root path where the peaks data is located.
#' @param subdir A character string specifying the subdirectory within the
#' root path where the peaks data is located,
#' defaults to "fragmentomics/processed".
#' @param parallelize A logical value indicating whether to
#' parallelize the loading of peaks data, defaults to TRUE.
#' @param number_of_daemons An integer specifying the number of
#' daemons to use for parallel processing,
#' defaults to the number of available cores.
#' @return A tibble containing the loaded peaks data.
#' @export
load_peaks <- function(experiment,
                       rootpath,
                       subdir = "fragmentomics/processed",
                       parallelize = TRUE,
                       number_of_daemons = parallel::detectCores()) {
  if (!file.exists(rootpath)) {
    stop("Root path does not exist: ", rootpath)
  }

  if (parallelize) {
    # FIXME can be also slurm
    # FIXME need a try catch
    mirai::daemons(number_of_daemons)
    res <- experiment |>
      dplyr::select(caseid:peakfile) |>
      purrr::pmap(
        purrr::in_parallel(
          \(caseid, sampleid, timepoint,
            encoded_timepoint, signal,
            target, source, peakfile) {
            # iterate in experiment rows
            fragmentomics::parse_peak_data(peakfile) |>
              dplyr::mutate(
                caseid = caseid,
                sampleid = sampleid,
                timepoint = timepoint,
                encoded_timepoint = encoded_timepoint,
                signal = signal,
                target = target,
                source = source,
                .before = 1
              )
          }
        )
      ) |>
      dplyr::bind_rows()
    mirai::daemons(0)
    res
  } else {
    experiment |>
      dplyr::select(caseid:peakfile) |>
      purrr::pmap(function(caseid, sampleid, timepoint,
                           encoded_timepoint, signal,
                           target, source, peakfile) {
        # iterate in experiment rows
        fragmentomics::parse_peak_data(peakfile) |>
          dplyr::mutate(
            caseid = caseid,
            sampleid = sampleid,
            timepoint = timepoint,
            encoded_timepoint = encoded_timepoint,
            signal = signal,
            target = target,
            source = source,
            .before = 1
          )
      }) |>
      dplyr::bind_rows()
  }
}
