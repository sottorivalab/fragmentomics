#' Create a list of matrices for each timepoint in the experiment
#' @param experiment A tibble containing the experiment data.
#' @param signal A character string specifying the signal to use for the matrix.
#' @param scale_mode A character string specifying the
#' scaling method to use for the matrix.
#' can be 'signal','target' or NA
#' @return A list of matrices, each corresponding to
#' a timepoint in the experiment.
#' @export
cohort_matrixes <- function(experiment,
                            signal = "central_coverage",
                            scale_mode = "signal") {

  # make matrix by timepoints
  heatmap_data <- experiment |>
    dplyr::group_split(timepoint) |>
    stats::setNames(unique(experiment$timepoint))

  matrix_data <- lapply(heatmap_data, function(hpdata) {
    x <- hpdata |>
      dplyr::select(signal_label, target_label, signal) |>
      dplyr::group_by(signal_label, target_label) |>
      tidyr::pivot_wider(names_from = signal_label,
                         values_from = signal)

    # this does not work because target_label is needed for group_by
    y <- x |>
      dplyr::ungroup() |>
      dplyr::select(-target_label) |>
      as.matrix()

    # set timepoint names
    row.names(y) <- x$target_label
    y
  })

  # scale by signals or targets
  if (scale_mode == "signal") {
    matrix_data <- lapply(matrix_data, function(m) {
      scale(m)
    })
  } else if (scale_mode == "target") {
    matrix_data <- lapply(matrix_data, function(m) {
      scale(t(m))
    })
  } else {
    matrix_data
  }
}
