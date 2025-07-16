#'
#' @title Plot Targets by Sample
#'
#' @description This function plots target data for
#' a specified target and source sample.

#' the color palette to use for the plot.
#'
#' @export
target_plot <- function(target, palette = "Set1") {
  # set colors
  palette <- RColorBrewer::brewer.pal(7, palette)
  maes <- ggplot2::aes(x = bin,
                       y = relative,
                       group = timepoint,
                       color = timepoint)

  mdata <- target |> dplyr::group_by(caseid)

  g <- ggplot2::ggplot(mdata, maes) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~caseid)
  g
}
