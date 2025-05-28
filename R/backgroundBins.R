#'
#' Calculate background mean using the limits of the distribution.
#' Aka the extreme parts of the peak that will represent the background
#'

backgroundBins <- function(data, left=50, right=50) {
  # calculate the extremes where left is X and right is L-Y
  mymax <- nrow(data) - right
  data |>
    dplyr::filter(bin <= left | bin >= mymax) |>
    dplyr::mutate(mean = mean(coverage)) |>
    dplyr::mutate(relative = coverage/mean(coverage))
}
