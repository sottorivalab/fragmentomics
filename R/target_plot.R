#'
#' @title Plot Targets by Sample
#'
#' @description This function plots target data for
#' a specified target and source sample.
#'
#' @param target A tibble containing target data with columns:
#' `caseid`, `sampleid`, `timepoint`, `encoded_timepoint`, `signal`,
#' `target`, `source`, `peakfile`, `bin`, `relative`.
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
#' ctcf <- load_peaks(experiment |> dplyr::filter(target == "CTCF"), "results")
#' target_plot(ctcf)
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

  # x scale
  # x_scale <- scale_x_bins(data[["average"]]$bin,
  #                         data[["stats"]]$central_bin,
  #                         data[["stats"]]$bin_size)
  #

  # FIXME fix scales
  g <- ggplot2::ggplot(mdata, maes) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~caseid)
    # ggplot2::scale_x_continuous(
    #   "Position relative to referencePoint (bp)",
    #   breaks = x_scale$breaks,
    #   labels = x_scale$labels
    # )
  g
}
