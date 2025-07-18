#' Plot Housekeeping Genes
#'
#' This function generates a ggplot plot for housekeeping genes.
#'
#' @param housekeeping A tibble containing the housekeeping gene peak data.
#' @param random A tibble containing the random datasets peak data.
#' @param relative A boolean indicating whether to plot relative coverage
housekeeping_plot <- function(housekeeping, random, relative = TRUE) {
  plot_title <- paste(
    unique(housekeeping$signal_label),
    "-",
    unique(housekeeping$target_label),
    "vs",
    length(unique(random$target_label)),
    "random sets")

  x_scale <- scale_x_bins(unique(housekeeping$bin),
                          unique(housekeeping$central_bin),
                          unique(housekeeping$bin_size))

  if (relative) {
    g <- housekeeping_plot_relative(housekeeping, random)
  } else {
    g <- housekeeping_plot_coverage(housekeeping, random)
  }

  g +
    ggplot2::ggtitle(plot_title) +
    ggplot2::scale_x_continuous(
      "Position relative to referencePoint (bp)",
      breaks = x_scale$breaks,
      labels = x_scale$labels
    ) +
    custom_legend(
      labels = c(unique(random$source_label), unique(housekeeping$target_label)),
      fill = c("grey", "red"),
      colour = NA,
      key = ggplot2::draw_key_polygon
    ) +
    ggplot2::theme(legend.position = "bottom")
}

#' Plot Relative Housekeeping Genes
#' @param housekeeping A tibble containing the housekeeping gene peak data.
#' @param random A tibble containing the random datasets peak data.
housekeeping_plot_relative <- function(housekeeping, random) {
  g <- ggplot2::ggplot() +
    ggplot2::geom_line(data = random,
                       ggplot2::aes(x = bin, y = relative),
                       color="grey", show.legend = FALSE) +
    ggplot2::geom_line(data = housekeeping,
                       ggplot2::aes(x = bin, y = relative),
                       color="red", show.legend = FALSE) +
    ggplot2::ylab("relative composite coverage")
  g
}

#' Plot Coverage Housekeeping Genes
#' @param housekeeping A tibble containing the housekeeping gene peak data.
#' @param random A tibble containing the random datasets peak data.
housekeeping_plot_coverage <- function(housekeeping, random) {
  g <- ggplot2::ggplot() +
    ggplot2::geom_line(data = random,
                       ggplot2::aes(x = bin, y = coverage),
                       color="grey", show.legend = FALSE) +
    ggplot2::geom_line(data = housekeeping,
                       ggplot2::aes(x = bin, y = coverage),
                       color="red", show.legend = FALSE) +
    ggplot2::ylab("composite coverage")
  g
}
