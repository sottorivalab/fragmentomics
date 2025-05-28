#' Summarise Compute Matrix
#'

summariseBins <- function(data) {
  data |>
    dplyr::summarise(across(everything(), \(x) mean(x, na.rm = TRUE))) |>
    tidyr::pivot_longer(colnames(y)) |>
    dplyr::rename(bin="name", coverage="value") |>
    dplyr::mutate(name = bin, .before=bin) |>
    dplyr::mutate(bin = dplyr::row_number(), .after=name)
}
