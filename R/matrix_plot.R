#' Plot a matrix of coverage data
#'
#' This function plots a matrix of coverage data using ComplexHeatmap.
#' @param matrix_data A data frame containing the matrix data to be plotted.
#' @param palette A character string representing the color palette
#' to be used for the plot.
#' @return A ComplexHeatmap object representing the plotted matrix.
#' @importFrom stats quantile
#' @export
matrix_plot <- function(matrix_data,
                        palette = "RdYlBu") {
  # stats
  peak_data <- peak_stats(matrix_data)

  # scale x
  scale_x <- scale_x_bins(peak_data$average$bin,
                          peak_data$stats$central_bin,
                          peak_data$stats$bin_size)
  # fix scale_x and set first to 1 not 0
  scale_x$breaks[1] <- 1

  # first sort by in descending order based on row mean
  ordered_data <- matrix_data |>
    dplyr::rowwise() |>
    dplyr::mutate(
      mean = mean(dplyr::c_across("bin_1":dplyr::last_col())),
      .before = 1
    ) |>
    dplyr::arrange(-mean)

  # matrix
  mat <- ordered_data |>
    dplyr::select("bin_1":dplyr::last_col()) |>
    as.matrix()

  # rownames are targets
  rownames(mat) <- ordered_data$name

  # try to avoid outliers by using quantile
  xmin <- min(mat, na.rm = TRUE)
  xmax <- quantile(mat, na.rm = TRUE, probs = c(.98))

  # color ramp
  col_fun <- circlize::colorRamp2(seq(xmin, xmax, length = 10),
                                  RColorBrewer::brewer.pal(n = 10,
                                                           name = palette))

  # coverage legend
  legend_param <- list(
    title = "coverage",
    legend_height = ggplot2::unit(4, "cm")
  )

  composite_line <- ComplexHeatmap::anno_lines(peak_data$average$coverage)
  ta <- ComplexHeatmap::HeatmapAnnotation(composite = composite_line,
                                          height = ggplot2::unit(2.5, "cm"))

  # bottom_annotation must be the scale
  basepairs_scale <- ComplexHeatmap::anno_mark(at = scale_x$breaks,
                                               labels = scale_x$labels,
                                               side = "bottom")

  ba <- ComplexHeatmap::HeatmapAnnotation(basepairs = basepairs_scale,
                                          height = ggplot2::unit(2, "cm"))


  h <- ComplexHeatmap::Heatmap(mat,
                               cluster_rows = FALSE,
                               cluster_columns = FALSE,
                               show_column_names = FALSE,
                               show_row_names = FALSE,
                               use_raster = TRUE,
                               raster_resize_mat = FALSE,
                               raster_quality = 10,
                               name = unique(matrix_data$target),
                               na_col = "black",
                               row_dend_reorder = FALSE,
                               column_dend_reorder = FALSE,
                               col = col_fun,
                               bottom_annotation = ba,
                               top_annotation = ta,
                               height = ggplot2::unit(10, "cm"),
                               width  = ggplot2::unit(3, "cm"),
                               heatmap_legend_param = legend_param)

  h
}
