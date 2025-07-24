#' Plot Housekeeping Genes
#'
#' This function generates a ggplot plot for housekeeping genes.
#'
#' @param housekeeping_data A tibble containing
#' the housekeeping_data gene peak data.
#' @param random_data A tibble containing the random_data datasets peak data.
#' @param relative A boolean indicating whether to plot relative coverage
#' @examples
#' \dontrun{
#' example_samplesheet <- system.file("extdata",
#'                                    "samplesheet.csv",
#'                                    package = "fragmentomics")
#' samplesheet <- parse_samplesheet(example_samplesheet)
#' experiment <- load_experiment(samplesheet, "results")
#' housekeeping <- experiment |>
#'   dplyr::filter(target_label == "GeneHancer_housekeeping",
#'                 sampleid == "SAMPLE_1_BL")
#' housekeeping_data <- load_peaks(housekeeping, "results")
#' random <- experiment |> dplyr::filter(source_label == "random_dataset",
#'                                       sampleid == "SAMPLE_1_BL")
#' random_data <- load_peaks(random, "results")
#' housekeeping_plot(housekeeping_data, random_data)
#' }
#' @export
housekeeping_plot <- function(housekeeping_data,
                              random_data,
                              relative = FALSE) {
  plot_subtitle <- paste(unique(housekeeping_data$target_label),
                         "vs",
                         length(unique(random_data$target_label)),
                         "random_data sets")

  x_scale <- scale_x_bins(unique(housekeeping_data$bin),
                          unique(housekeeping_data$central_bin),
                          unique(housekeeping_data$bin_size))

  if (relative) {
    g <- housekeeping_plot_relative(housekeeping_data, random_data)
  } else {
    g <- housekeeping_plot_coverage(housekeeping_data, random_data)
  }

  g +
    ggplot2::ggtitle(unique(housekeeping_data$signal_label),
                     subtitle = plot_subtitle) +
    ggplot2::scale_x_continuous(
      "Position relative to referencePoint (bp)",
      breaks = x_scale$breaks,
      labels = x_scale$labels
    ) +
    custom_legend(
      labels = c(unique(random_data$source_label),
                 unique(housekeeping_data$target_label)),
      fill = c("grey", "red"),
      colour = NA,
      key = ggplot2::draw_key_polygon
    ) +
    ggplot2::theme(legend.position = "bottom")
}

#' Plot Relative Housekeeping Genes
#' @param housekeeping_data A tibble containing the
#' housekeeping_data gene peak data.
#' @param random_data A tibble containing the
#' random_data datasets peak data.
housekeeping_plot_relative <- function(housekeeping_data, random_data) {
  g <- ggplot2::ggplot() +
    ggplot2::geom_line(data = random_data,
                       ggplot2::aes(x = bin, y = relative),
                       color = "grey", show.legend = FALSE) +
    ggplot2::geom_line(data = housekeeping_data,
                       ggplot2::aes(x = bin, y = relative),
                       color = "red", show.legend = FALSE) +
    ggplot2::ylab("relative composite coverage")
  g
}

#' Plot Coverage Housekeeping Genes
#' @param housekeeping_data A tibble containing the
#' housekeeping_data gene peak data.
#' @param random_data A tibble containing the random_data datasets peak data.
housekeeping_plot_coverage <- function(housekeeping_data, random_data) {
  g <- ggplot2::ggplot() +
    ggplot2::geom_line(data = random_data,
                       ggplot2::aes(x = bin, y = coverage),
                       color = "grey", show.legend = FALSE) +
    ggplot2::geom_line(data = housekeeping_data,
                       ggplot2::aes(x = bin, y = coverage),
                       color = "red", show.legend = FALSE) +
    ggplot2::ylab("composite coverage")
  g
}
