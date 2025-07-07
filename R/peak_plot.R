#' Peak Plot
#'
#' This function generates a ggplot plot for the given data.
#'
#' @param data A data frame containing the data to be plotted.
#' @param normalized A logical value indicating whether the data is normalized.
#' If TRUE, the plot will show relative signal;
#' if FALSE, it will show raw signal.
#' @return A ggplot object.
#'
#' @export
peak_plot <- function(data, normalized = FALSE) {
  if (normalized) {
    peak_relative_length <- tibble::tibble(
      x = max(data[["average"]]$bin),
      y = c(
        data[["stats"]]$reference_point_relative,
        1
      )
    )
    x_scale = scale_x_bins(data)

    # plot relative signal
    ggplot2::ggplot() +
      # composite coverage
      ggplot2::geom_line(
        data = data[["average"]],ggplot2::aes(y=relative, x=bin)
      ) +
      ggplot2::geom_hline(
        yintercept = 1, color = "#6082B6", linetype = "dotted"
      ) +
      # length
      ggplot2::geom_point(
        data = peak_relative_length,
        ggplot2::aes(x = x, y = y), color = "green", size = 1
      ) +
      ggplot2::geom_line(
        data = peak_relative_length,
        ggplot2::aes(x = x, y = y),
        color = "green", linetype = "dashed"
      ) +
      # labels
      ggplot2::xlab(data[["stats"]]$target_label) +
      ggplot2::ylab(data[["stats"]]$signal_label) +
      ggplot2::ggtitle(
        paste(
              "Composite coverage: ",
              data[["stats"]]$target_label,
              "on", data[["stats"]]$signal_label, sep = " "),
        subtitle = paste(
          "relative=",
          round(data[["stats"]]$reference_point_relative, digits = 4),
          " relative length=",
          round(data[["stats"]]$peak_relative, digits = 4),
          sep = ""
        )
      ) +
      ggplot2::scale_x_continuous(
        "Position relative to referencePoint (bp)",
        breaks = x_scale$breaks,
        labels = x_scale$labels
      ) +
      ggplot2::theme(legend.position = "none")
  } else{
    # labels
    label_pos <- -(max(data[["average"]]$bin) * .1)
    x_scale <- scale_x_bins(data)
    # peak length
    peak_length <- tibble::tibble(
      x = max(data[["average"]]$bin),
      y = c(
        data[["stats"]]$reference_point_coverage,
        data[["stats"]]$background_mean
      )
    )
    # plot raw signal
    g1 <- ggplot2::ggplot() +
      # composite coverage
      ggplot2::geom_line(
        data = data[["average"]],
        ggplot2::aes(y = coverage, x = bin)
      ) +
      # integration data
      ggplot2::geom_point(
        data = data[["integration"]],
        ggplot2::aes(x = x, y = y, color = above),
        size = .2,
        alpha = .5
      ) +
      ggplot2::scale_color_manual(values = c("#D3D3D3", "#56B4E9")) +
      # background mean
      ggplot2::geom_hline(
        yintercept = data[["stats"]]$background_mean,
        color = "#6082B6", linetype = "dotted"
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label_pos, data[["stats"]]$background_mean,
                     label = "background"),
        color = "#6082B6", size = 2.5, vjust = -1
      ) +
      ggplot2::geom_rect(
        ggplot2::aes(
          xmin = 0, xmax = data[["stats"]]$background_left_limit,
          ymin = min(data[["average"]]$coverage),
          ymax = max(data[["average"]]$coverage)
        ),
        color = "#393f47",
        alpha = .15
      ) +
      ggplot2::geom_rect(
        ggplot2::aes(
          xmin = data[["stats"]]$background_right_limit,
          xmax = max(data[["average"]]$bin),
          ymin = min(data[["average"]]$coverage),
          ymax = max(data[["average"]]$coverage)
        ),
        color = "#6082B6",
        alpha = .15
      ) +
      # reference coverage
      ggplot2::geom_hline(
        yintercept = data[["stats"]]$reference_point_coverage,
        color = "#6082B6",
        linetype = "dotted"
      ) +
      ggplot2::geom_text(
        ggplot2::aes(
          label_pos, data[["stats"]]$reference_point_coverage,
          label = "reference"
        ), size = 2.5, vjust = 1, color = "#6082B6"
      ) +
      # central coverage
      ggplot2::geom_hline(
        yintercept = data[["stats"]]$central_coverage,
        color = "orange",
        linetype = "dotted"
      ) +
      ggplot2::geom_text(
        ggplot2::aes(
          label_pos, data[["stats"]]$central_coverage, label = "central"
        ),
        size = 2.5,
        vjust = -1,
        color = "orange"
      ) +
      # central coverage limits
      ggplot2::geom_rect(
        ggplot2::aes(
          xmin = data[["stats"]]$central_coverage_bin_min,
          xmax = data[["stats"]]$central_coverage_bin_max,
          ymin = min(data[["average"]]$coverage),
          ymax = max(data[["average"]]$coverage)
        ),
        alpha = .25,
        color = "orange",
        fill = "orange"
      ) +
      # average coverage
      ggplot2::geom_hline(
        yintercept = data[["stats"]]$average_coverage,
        color = "red", linetype = "dotted"
      ) +
      ggplot2::geom_text(
        ggplot2::aes(
                     label_pos,
                     data[["stats"]]$average_coverage,
                     label = "average"),
        size = 2.5,
        vjust = -1,
        color = "red"
      ) +
      # average coverage limits
      ggplot2::geom_rect(
        ggplot2::aes(
          xmin = data[["stats"]]$average_coverage_bin_min,
          xmax = data[["stats"]]$average_coverage_bin_max,
          ymin = min(data[["average"]]$coverage),
          ymax = max(data[["average"]]$coverage)
        ),
        alpha = .15,
        color = "red",
        fill = "red"
      ) +
      # length
      ggplot2::geom_point(
        data = peak_length,
        ggplot2::aes(x = x, y = y),
        color = "green",
        size = 1
      ) +
      ggplot2::geom_line(
        data=peak_length,
        ggplot2::aes(x = x, y = y),
        color = "green",
        linetype = "dashed"
      ) +
      # labels
      ggplot2::xlab(data[["stats"]]$target_label) +
      ggplot2::ylab(data[["stats"]]$signal_label) +
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
      ggplot2::theme(legend.position = "none")

    # return plot
    return(g1)
  }
}

#'
#' Function to build the x scale based on nother of bins
#' @param data A list containing the average data and stats.
#' @return A list with breaks and labels for the x scale.
scale_x_bins <- function(data) {
  bins <- table(ggplot2::cut_number(data[["average"]]$bin,8, labels = FALSE))
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
