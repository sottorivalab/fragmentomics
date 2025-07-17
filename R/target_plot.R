#'
#' @title Plot Targets by Sample
#'
#' @description This function plots target data for
#' a specified target and source sample.
#'
#' @param target A tibble containing target data
#' @param palette A character string specifying the color palette to use.
#' Defaults to "Set1" from RColorBrewer.
#' @return A ggplot object visualizing the target data.
#' @examples
#' \dontrun{
#' example_samplesheet <- system.file("extdata",
#'                                    "samplesheet.csv",
#'                                    package = "fragmentomics")
#' samplesheet <- parse_samplesheet(example_samplesheet)
#' experiment <- load_experiment(samplesheet, "results")
#' ctcf <- experiment |> dplyr::filter(target_label == "CTCF")
#' ctcf_peaks <- load_peaks(, "results")
#' target_plot(ctcf_peaks)
#' }
#' @export
target_plot <- function(target, palette = "Set1") {
  # set colors
  palette <- RColorBrewer::brewer.pal(7, palette)

  maes <- ggplot2::aes(x = bin,
                       y = relative,
                       group = timepoint,
                       color = timepoint)

  mdata <- target |> dplyr::group_by(caseid)

  # FIXME fix scales I need consistent TEST data
  x_scale <- scale_x_bins(unique(target$bin),
                          unique(target$central_bin),
                          unique(target$bin_size))

  g <- ggplot2::ggplot(mdata, maes) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~caseid) +
    ggplot2::scale_x_continuous(
      "Position relative to referencePoint (bp)",
      breaks = x_scale$breaks,
      labels = x_scale$labels
    )
  g
}
