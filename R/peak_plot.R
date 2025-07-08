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

#'
#' scale_x_bins
#'
#' Function to build the x scale based on nother of bins
#' @param data A list containing the average data and stats.
#' @return A list with breaks and labels for the x scale.
scale_x_bins <- function(data) {
  bins <- table(ggplot2::cut_number(data[["average"]]$bin, 8, labels = FALSE))
  breaks <- as.numeric(c(0, cumsum(bins)))
  # the central referencePoint
  central_bin <- data[["stats"]]$central_bin
  labels <- lapply(breaks, function(br) {
    (br - central_bin) * data[["stats"]]$bin_size
  })
  labels <- lapply(labels, label_basepairs)
  list(
    breaks = breaks,
    labels = unlist(labels)
  )
}

#'
#' Label basepairs
#'
#' @param x A numeric vector representing basepairs.
#' @return A character vector with basepairs labeled with appropriate suffixes.
#'
label_basepairs <- function(x) {
  breaks <- c(0, 10^c("k" = 3, "M" = 6, "G" = 9, "T" = 12))
  n_suffix <- cut(abs(x),
                  breaks = c(unname(breaks), Inf),
                  labels = c(names(breaks)),
                  right = FALSE)
  n_suffix[is.na(n_suffix)] <- ""
  # Don't suffix basepairs unless >= kb
  suffix <- ifelse(nzchar(as.character(n_suffix)),
                   paste0("", n_suffix, "b"), "")
  scale <- 1 / breaks[n_suffix]
  scale[which(scale %in% c(Inf, NA))] <- 1
  paste(x * scale, suffix, sep = "")
}

#'
#' custom_legend
#' Function to create a custom legend for ggplot2.
#' NOTE: the labels are reorder in alphabetical order.
#'
#' @param labels A character vector of labels for the legend.
#' @param ... A variable number of aesthetic mappings to be used in the legend.
#' @param title A character string for the title of the legend.
#' @param key A function to draw the key glyph
#' in the legend (default is `draw_key_point`).
#' @param guide_args A list of additional arguments
#' to be passed to the `guide_legend` function.
#' @return A list containing a dummy geom and a dummy scale for the legend.
custom_legend <- function(labels = NULL,
                          ...,
                          title = NULL,
                          key   = ggplot2::draw_key_point,
                          guide_args = list()) {
  # Capture arguments
  aesthetics <- list(...)
  n <- max(lengths(aesthetics), 0)
  labels <- labels %||%  seq_len(n)

  # Overrule the alpha = 0 that we use to hide the points
  aesthetics$alpha <- aesthetics$alpha %||% rep(1, n)

  # Construct guide
  guide_args$override.aes <- guide_args$override.aes %||% aesthetics
  guide <- do.call(ggplot2::guide_legend, guide_args)

  # Allow dummy aesthetic
  ggplot2::update_geom_defaults("point", list(dummy = "x"))

  dummy_geom <- ggplot2::geom_point(
    data = data.frame(x = rep(Inf, n), y = rep(Inf, n),
                      dummy = factor(labels)),
    ggplot2::aes(x, y, dummy = dummy), alpha = 0, key_glyph = key
  )

  dummy_scale <- ggplot2::discrete_scale(
    "dummy", palette = scales::identity_pal(), name = title,
    guide = guide
  )

  list(dummy_geom, dummy_scale)
}
