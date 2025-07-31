#' Create a list of matrices for each timepoint in the experiment
#' @param experiment A tibble containing the experiment data.
#' @param signal A character string specifying the signal to use for the matrix.
#' @return A list of matrices, each corresponding to
#' a timepoint in the experiment.
#' @export
cohort_matrixes <- function(experiment,
                            signal = "central_coverage") {

  # make matrix by timepoints
  heatmap_data <- experiment |>
    dplyr::group_split(timepoint) |>
    stats::setNames(unique(experiment$timepoint))

  matrix_data <- lapply(heatmap_data, function(hpdata) {
    x <- hpdata |>
      dplyr::select(signal_label, target_label, signal) |>
      dplyr::group_by(signal_label) |>
      tidyr::pivot_wider(names_from = signal_label,
                         values_from = signal)
    y <- x |>
      dplyr::select(-target_label) |>
      as.matrix()

    # set timepoint names
    row.names(y) <- x$target_label

    # scale HERE
    scale(y)
  })

  matrix_data
}
