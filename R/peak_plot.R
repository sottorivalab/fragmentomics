#' Peak Plot
#'
#' This function generates a ggplot plot for the given data.
#'
#' @param data A data frame containing the data to be plotted.
#' @param normalized A logical value indicating whether the data is normalized.
#' If TRUE, the plot will show relative signal;
#' if FALSE, it will show raw signal.
#' @param palette A character string representing the
#' color palette to be used for the plot.
#' @return A ggplot object.
#'
#' @examples
#' file <- system.file("extdata", "CTCF.gz", package = "fragmentomics")
#' matrix_data <- parse_compute_matrix(file)
#' peak_data <- peak_stats(matrix_data)
#' peak_plot(peak_data)
#' peak_plot(peak_data, normalized=TRUE)
#'
#' @export
peak_plot <- function(data, normalized = FALSE, palette = "Set1") {
  # set colors
  palette <- RColorBrewer::brewer.pal(7, palette)

  ifelse(
    normalized,
    return(normalized_plot(data, palette)),
    return(raw_plot(data, palette))
  )
}


#' Normalized plot
#'
#' This function generates a ggplot plot for normalized data.
#'
#' @param data A list containing the average data and stats.
#' @param palette A character vector representing the color
#' palette to be used for the plot.A character vector
#' representing the color palette to be used for the plot.
#' @return A ggplot object for normalized data.
normalized_plot <- function(data, palette) {

  # title
  title_label <- paste(
    "Composite coverage: ",
    data[["stats"]]$target_label,
    "on",
    data[["stats"]]$signal_label,
    sep = " "
  )

  # subtitle
  subtitle_label <- paste(
    "reference_point_relative=",
    round(data[["stats"]]$reference_point_relative, digits = 4),
    " - peak_relative_length=",
    round(data[["stats"]]$peak_relative, digits = 4),
    sep = ""
  )

  # x scale
  x_scale <- scale_x_bins(data)

  # plot relative signal
  g <- ggplot2::ggplot() +
    # composite normalized coverage
    ggplot2::geom_line(
      data = data[["average"]],
      ggplot2::aes(
        y = relative,
        x = bin
      ),
      color = palette[1]
    ) +
    # background line on 1 (relative signal)
    ggplot2::geom_hline(
      yintercept = 1, color = palette[2], linetype = "dotted"
    ) +
    # background rect
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = 0, xmax = data[["stats"]]$background_left_limit,
        ymin = min(data[["average"]]$relative),
        ymax = max(data[["average"]]$relative)
      ),
      color = palette[2],
      alpha = .15
    ) +
    # background rect
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = data[["stats"]]$background_right_limit,
        xmax = max(data[["average"]]$bin),
        ymin = min(data[["average"]]$relative),
        ymax = max(data[["average"]]$relative)
      ),
      color = palette[2],
      alpha = .15
    ) +
    # labels
    ggplot2::xlab(data[["stats"]]$target_label) +
    ggplot2::ylab(data[["stats"]]$signal_label) +
    ggplot2::ggtitle(title_label, subtitle = subtitle_label) +
    ggplot2::scale_x_continuous(
      "Position relative to referencePoint (bp)",
      breaks = x_scale$breaks,
      labels = x_scale$labels
    ) +
    custom_legend(
      labels = c("Background coverage", "Composite coverage"),
      fill = c(palette[2], palette[1]),
      colour = NA,
      key = ggplot2::draw_key_polygon
    ) +
    ggplot2::theme(legend.position = "right")
  g
}

#' Raw plot
#'
#' This function generates a ggplot plot for raw data.
#'
#' @param data A list containing the average data, integration data, and stats.
#' @param palette A character string representing the color palette
#' to be used for the plot.
#' @return A ggplot object for raw data.
raw_plot <- function(data, palette) {

  # labels
  label_pos <- -(max(data[["average"]]$bin) * .1)

  # x scale
  x_scale <- scale_x_bins(data)


  # calculate space for geom_rect on top as max + 10%
  data_range <- range(data[["average"]]$coverage)
  data_inc <- (data_range[2] - data_range[1]) * .025
  top_rect_min <- data_range[2] + data_inc
  top_rect_max <- top_rect_min + data_inc

  # plot raw signal
  g <- ggplot2::ggplot() +
    # composite coverage
    ggplot2::geom_line(
      data = data[["average"]],
      ggplot2::aes(y = coverage, x = bin),
      color = palette[1]
    ) +
    # integration data
    ggplot2::geom_point(
      data = data[["integration"]],
      ggplot2::aes(x = x, y = y, color = above),
      size = .5,
      alpha = .25
    ) +
    ggplot2::scale_color_manual(
      values = c("#D3D3D3", palette[3]),
      labels = c("outside", "inside")
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        title = "Integration",
        override.aes = list(size = 3)
      )
    ) +
    # background mean
    ggplot2::geom_hline(
      yintercept = data[["stats"]]$background_mean,
      color = palette[2], linetype = "dotted"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label_pos, data[["stats"]]$background_mean,
                   label = "background"),
      color = "black",
      size = 2,
      vjust = -0.5
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = 0, xmax = data[["stats"]]$background_left_limit,
        ymin = min(data[["average"]]$coverage),
        ymax = max(data[["average"]]$coverage)
      ),
      color = palette[2],
      fill = palette[2],
      alpha = .15
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = data[["stats"]]$background_right_limit,
        xmax = max(data[["average"]]$bin),
        ymin = min(data[["average"]]$coverage),
        ymax = max(data[["average"]]$coverage)
      ),
      color = palette[2],
      fill = palette[2],
      alpha = .15
    ) +
    # reference coverage
    ggplot2::geom_hline(
      yintercept = data[["stats"]]$reference_point_coverage,
      color = palette[4],
      linetype = "dotted"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label_pos, data[["stats"]]$reference_point_coverage,
        label = "reference"
      ),
      size = 2,
      vjust = -0.5,
      color = palette[4]
    ) +
    # average coverage
    ggplot2::geom_hline(
      yintercept = data[["stats"]]$average_coverage,
      color = palette[7],
      linetype = "dotted"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label_pos,
        data[["stats"]]$average_coverage,
        label = "average"
      ),
      size = 2,
      vjust = -0.5,
      color = palette[7]
    ) +
    # average coverage limits
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = data[["stats"]]$average_coverage_bin_min,
        xmax = data[["stats"]]$average_coverage_bin_max,
        ymin = top_rect_min + 0.1,
        ymax = top_rect_max - 0.1
      ),
      alpha = .15,
      color = palette[7],
      fill = palette[7]
    ) +
    # central coverage
    ggplot2::geom_hline(
      yintercept = data[["stats"]]$central_coverage,
      color = palette[5],
      linetype = "dotted"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label_pos, data[["stats"]]$central_coverage, label = "central"
      ),
      size = 2,
      vjust = -0.5,
      color = palette[5]
    ) +
    # central coverage limits
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = data[["stats"]]$central_coverage_bin_min,
        xmax = data[["stats"]]$central_coverage_bin_max,
        ymin = top_rect_min,
        ymax = top_rect_max
      ),
      alpha = .25,
      fill = palette[5],
      color = palette[5]
    ) +
    # labels
    ggplot2::xlab(data[["stats"]]$target_label) +
    ggplot2::ylab(data[["stats"]]$signal_label) +
    # custom legend
    custom_legend(
      title = "Metrics",
      labels = c(
        "Average coverage",
        "Background coverage",
        "Central coverage",
        "Composite coverage",
        "Reference coverage"
      ),
      fill = c(palette[6], palette[2], palette[5], palette[1], palette[4]),
      colour = NA,
      key = ggplot2::draw_key_polygon
    ) +
    ggplot2::ggtitle(
      paste(
        "Composite coverage:",
        data[["stats"]]$target_label,
        "on",
        data[["stats"]]$signal_label,
        sep = " "
      ),
      subtitle = paste(
        "reference=",
        round(data[["stats"]]$reference_point_coverage, digits = 4),
        " central=",
        round(data[["stats"]]$central_coverage, digits = 4),
        " average=",
        round(data[["stats"]]$average_coverage, digits = 4),
        " background=",
        round(data[["stats"]]$background_mean, digits = 4),
        " length=",
        round(data[["stats"]]$peak_length, digits = 4),
        sep = ""
      )
    ) +
    ggplot2::scale_x_continuous(
      "Position relative to referencePoint (bp)",
      breaks = x_scale$breaks,
      labels = x_scale$labels
    ) +
    ggplot2::theme(legend.position = "right")

  g
}
