#'
#' @title Plot Targets by Sample
#'
#' @description This function plots target data for a specified target and source sample.
#' @param experiment An experiment object containing target data.
#' @param target A character string specifying the target to plot.
#' @param source A character string specifying the source sample to plot.
#'
target_plot <- function(experiment, target, source, palette = "Set1") {
  # set colors
  palette <- RColorBrewer::brewer.pal(7, palette)
  mdata <- experiment$targets[[source]] |>
    dplyr::filter(targetid == target) |>
    dplyr::group_by(caseid)

  g <- ggplot2::ggplot(mdata, ggplot2::aes(x = bin, y = relative, group=timepoint, color=timepoint)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~caseid)

  g
}
