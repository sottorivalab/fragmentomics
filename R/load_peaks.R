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
#' @examples
#' \dontrun{
#' example_samplesheet <- system.file("extdata",
#'                                    "samplesheet.csv",
#'                                    package = "fragmentomics")
#' samplesheet <- parse_samplesheet(example_samplesheet)
#' experiment <- load_experiment(samplesheet, "results")
#' ctcf <- experiment |> dplyr::filter(target_label == "CTCF")
#' ctcf_peaks <- load_peaks(ctcf, "results/")
#' }
#' @export
load_peaks <- function(experiment,
                       rootpath,
                       subdir = "fragmentomics/processed",
                       parallelize = TRUE,
                       number_of_daemons = parallel::detectCores()) {

  if (!file.exists(rootpath)) {
    stop("Root path does not exist: ", rootpath)
  }

  mdata <- experiment |> dplyr::select(caseid:peakfile, bin_size, central_bin)

  if (parallelize) {
    # FIXME can be also slurm
    mirai::daemons(number_of_daemons)
    tryCatch(
      res <- parallel_load_peaks(mdata),
      finally = {
        mirai::daemons(0)
      }
    )
    res
  } else {
    mdata |>
      purrr::pmap(function(caseid, sampleid, timepoint,
                           encoded_timepoint, signal_label,
                           target_label, source_label, peakfile,
                           bin_size, central_bin) {
        fragmentomics::load_peak_data(caseid, sampleid,
                                      timepoint, encoded_timepoint,
                                      signal_label, target_label, source_label,
                                      bin_size, central_bin, peakfile)
      }) |>
      dplyr::bind_rows()
  }
}

#' parallel_load_peaks
#' Load peaks data in parallel.
#' @examples#'
#' @param mdata A tibble containing the metadata for the peaks data.
#' @return A tibble containing the loaded peaks data.
#' @export
parallel_load_peaks <- function(mdata) {
  mdata |>
    purrr::pmap(purrr::in_parallel(
      \(caseid, sampleid, timepoint,
        encoded_timepoint, signal_label,
        target_label, source_label,
        peakfile, bin_size, central_bin) {
        fragmentomics::load_peak_data(caseid, sampleid,
                                      timepoint, encoded_timepoint,
                                      signal_label, target_label, source_label,
                                      bin_size, central_bin, peakfile)
      }
    )) |>
    dplyr::bind_rows()
}


#' load_peak_data
#' Load peak data from a specified file and add metadata.
#'
#' @param caseid A character string representing the case ID.
#' @param sampleid A character string representing the sample ID.
#' @param timepoint A factor representing the timepoint.
#' @param encoded_timepoint An integer representing the encoded timepoint.
#' @param signal_label A character string representing the signal label.
#' @param target_label A character string representing the target label.
#' @param source_label A character string representing the source label.
#' @param bin_size An integer representing the size of
#' the bins used in the peak data.
#' @param central_bin An integer representing the central bin of the peak data.
#' @param peakfile A character string representing
#' the path to the peak data file.
#'
#' @return A tibble containing the parsed peak data with additional metadata.
#' @export
load_peak_data <- function(caseid,
                           sampleid,
                           timepoint,
                           encoded_timepoint,
                           signal_label,
                           target_label,
                           source_label,
                           bin_size,
                           central_bin,
                           peakfile) {
  fragmentomics::parse_peak_data(peakfile) |>
    dplyr::mutate(
      caseid = caseid,
      sampleid = sampleid,
      timepoint = timepoint,
      encoded_timepoint = encoded_timepoint,
      signal_label = signal_label,
      target_label = target_label,
      source_label = source_label,
      bin_size = bin_size,
      central_bin = central_bin,
      .before = 1
    )
}
