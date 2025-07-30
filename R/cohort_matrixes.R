#' @export
cohort_matrixes <- function(experiment, source_label) {
  # make matrix by timepoints
  heatmap_data <- experiment |>
    dplyr::filter(source_label == source_label) |>
    dplyr::group_split(timepoint) |>
    stats::setNames(unique(experiment$timepoint))

  matrix_data <- lapply(heatmap_data, function(hpdata){
    x <- hpdata |>
      dplyr::select(signal_label, target_label, central_coverage) |>
      dplyr::group_by(signal_label) |>
      tidyr::pivot_wider(names_from = signal_label,
                         values_from = central_coverage)
    y <- x |>
      dplyr::select(-target_label) |>
      as.matrix()
    row.names(y) <- x$target_label

    # scale HERE
    scale(y)
  })

  matrix_data
}
