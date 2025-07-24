#' Housekeeping Z-score Plot
#'
#' This function generates a ggplot plot for the z-scores of housekeeping genes
#' compared to random datasets.
#' @param housekeeping A tibble containing the housekeeping gene stats data.
#' @param random A tibble containing the random datasets stats data.
#' @param bins An optional integer specifying
#' the number of bins for the histogram.
#' If NULL, breaks are calculated using Sturge's method.
#' @return A ggplot object for the z-scores of housekeeping genes.
#' @export
housekeeping_zscore_plot <- function(housekeeping, random, bins = NULL) {

  s <- housekeeping_zscore(housekeeping, random)
  plot_title <- housekeeping$sampleid
  plot_subtitle <- paste("zscore=",
                         round(s$z, digits = 2),
                         " p=",
                         s$p,
                         sep = "")
  if (is.null(bins)) {
    breaks <- pretty(range(random$peak_length),
                     n = nclass.Sturges(random$peak_length),
                     min.n = 1)
    g <- ggplot2::ggplot(random, ggplot2::aes(x = peak_length)) +
      ggplot2::geom_histogram(color = "darkblue",
                              fill = "lightblue",
                              breaks = breaks) +
      ggplot2::geom_density(color = "blue")
  } else {
    g <- ggplot2::ggplot(random,
                         ggplot2::aes(x = peak_length)) +
      ggplot2::geom_histogram(color = "darkblue",
                              fill = "lightblue",
                              bins = bins) +
      ggplot2::geom_density(color = "blue")
  }

  b <- ggplot2::ggplot_build(g)

  garrow <- ggplot2::arrow(length = ggplot2::unit(0.25, "cm"))

  g1 <- g +
    ggplot2::ggtitle(plot_title, subtitle = plot_subtitle) +
    ggrepel::geom_label_repel(data = housekeeping,
                              ggplot2::aes(x = peak_length,
                                           y = max(b$data[[1]]$count)),
                              color = "red",
                              label = "Housekeeping TSS",
                              nudge_x = 0, nudge_y = 1.5) +
    ggplot2::geom_segment(data = housekeeping,
                          ggplot2::aes(x = peak_length, xend = peak_length),
                          y = max(b$data[[1]]$count),
                          yend = 0,
                          color = "red",
                          arrow = garrow) +
    ggplot2::ylab("random TSS")

  g1
}
