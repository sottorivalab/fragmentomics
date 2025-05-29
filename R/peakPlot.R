#' Peak Plot
#'
#' This function generates a ggplot plot for the given data.
#'
#' @param data A data frame containing the data to be plotted.
#' @return A ggplot object.
#'
#' @export
peakPlot <- function(data, normalized = FALSE) {
  if (normalized) {
    peak_relative_length <- tibble::tibble(
      x = max(data[['average']]$bin),
      y=c(
        data[['stats']]$reference_point_relative,
        1
      )
    )

    # plot relative signal
    ggplot2::ggplot() +
      # composite coverage
      ggplot2::geom_line(data=data[['average']],ggplot2::aes(y=relative, x=bin)) +
      ggplot2::geom_hline(yintercept = 1, color="#6082B6", linetype = 'dotted') +
      # length
      ggplot2::geom_point(data=peak_relative_length, ggplot2::aes(x=x, y=y), color="green", size=1) +
      ggplot2::geom_line(data=peak_relative_length, ggplot2::aes(x=x, y=y), color="green", linetype="dashed") +
      # labels
      ggplot2::xlab(data[['stats']]$target_label) +
      ggplot2::ylab(data[['stats']]$signal_label) +
      ggplot2::ggtitle(
        paste("Composite coverage: ", data[['stats']]$target_label, "on", data[['stats']]$signal_label, sep=" "),
        subtitle = paste(
          "relative=",
          round(data[['stats']]$reference_point_relative, digits=4),
          " relative length=",
          round(data[['stats']]$peak_relative, digits=4),
          sep=""
        )
      ) +
      # FIXME make it relative to number of bins
      ggplot2::scale_x_continuous(
        "Position relative to referencePoint (bp)",
        breaks = c(0,100,200,300,400,500,600,700,800),
        labels = c("-4kb","-3kb","-2kb","-1kb","0","1kb","2kb","3kb","4kb")
      ) +
      ggplot2::theme(legend.position = "none")
  } 
  else 
  {
    # labels
    label.pos <- -(max(data[['average']]$bin) * .1)
    # peak length
    peak.length <- tibble::tibble(
      x=max(data[['average']]$bin),
      y=c(
        data[['stats']]$reference_point_coverage,
        data[['stats']]$background_mean
      )
    )
    # plot
    # plot raw signal
    g1 <- ggplot2::ggplot() +
      # composite coverage
      ggplot2::geom_line(
        data=data[['average']],
        ggplot2::aes(y=coverage, x=bin)
      ) +
      # integration data
      ggplot2::geom_point(
        data=data[['integration']],
        ggplot2::aes(x = x, y = y, color=above),
        size = .2,
        alpha = .5
      ) +
      ggplot2::scale_color_manual(values=c("#D3D3D3", "#56B4E9")) +
      # background mean
      ggplot2::geom_hline(yintercept = data[['stats']]$background_mean, color="#6082B6", linetype = 'dotted') +
      ggplot2::geom_text(ggplot2::aes(label.pos,data[['stats']]$background_mean,label = "background"), color="#6082B6", size = 2.5, vjust = -1) +
      ggplot2::geom_rect(ggplot2::aes(xmin = 0,xmax = data[['stats']]$background_left_limit,  ymin=min(data[['average']]$coverage),ymax=max(data[['average']]$coverage)),
        color="#6082B6",
        alpha = .15
      ) +
      ggplot2::geom_rect(
        ggplot2::aes(
          xmin = data[['stats']]$background_right_limit,
          xmax = max(data[['average']]$bin),
          ymin=min(data[['average']]$coverage),
          ymax=max(data[['average']]$coverage)
        ),
        color="#6082B6",
        alpha = .15
      ) +
      # reference coverage
      ggplot2::geom_hline(yintercept = data[['stats']]$reference_point_coverage, color="#6082B6", linetype = 'dotted') +
      ggplot2::geom_text(ggplot2::aes(label.pos,data[['stats']]$reference_point_coverage,label = "reference"), size = 2.5, vjust = 1, color="#6082B6") +
      # central coverage
      ggplot2::geom_hline(yintercept = data[['stats']]$central_coverage, color="orange", linetype = 'dotted') +
      ggplot2::geom_text(ggplot2::aes(label.pos,data[['stats']]$central_coverage,label = "central"), size = 2.5, vjust = -1, color="orange") +
      # central coverage limits
      ggplot2::geom_rect(
        ggplot2::aes(
          xmin = data[['stats']]$central_coverage_bin_min,
          xmax = data[['stats']]$central_coverage_bin_max,
          ymin=min(data[['average']]$coverage),
          ymax=max(data[['average']]$coverage)
        ),
        alpha=.25,
        color="orange",
        fill="orange"
      ) +
      # average coverage
      ggplot2::geom_hline(yintercept = data[['stats']]$average_coverage, color="red", linetype = 'dotted') +
      ggplot2::geom_text(ggplot2::aes(label.pos,data[['stats']]$average_coverage,label = "average"), size = 2.5, vjust = -1, color="red") +
      # average coverage limits
      ggplot2::geom_rect(
        ggplot2::aes(
          xmin = data[['stats']]$average_coverage_bin_min,
          xmax = data[['stats']]$average_coverage_bin_max,
          ymin=min(data[['average']]$coverage),
          ymax=max(data[['average']]$coverage)
        ),
        alpha=.15,
        color="red",
        fill="red"
      ) +
      # length
      ggplot2::geom_point(data=peak.length, ggplot2::aes(x=x, y=y), color="green", size=1) +
      ggplot2::geom_line(data=peak.length, ggplot2::aes(x=x, y=y), color="green", linetype="dashed") +
      # labels
      ggplot2::xlab(data[['stats']]$target_label) +
      ggplot2::ylab(data[['stats']]$signal_label) +
      ggplot2::ggtitle(
        paste("Composite coverage: ", data[['stats']]$target_label, "on", data[['stats']]$signal_label, sep=" "),
        subtitle = paste(
          "reference=",
          round(data[['stats']]$reference_point_coverage, digits=4),
          " central=",
          round(data[['stats']]$central_coverage, digits=4),
          " average=",
          round(data[['stats']]$average_coverage, digits=4),
          " background=",
          round(data[['stats']]$background_mean,digits=4),
          " length=",
          round(data[['stats']]$peak_length,digits=4),
          sep=""
        )
      ) +
      # FIXME make it relative to number of bins
      ggplot2::scale_x_continuous(
        "Position relative to referencePoint (bp)",
        breaks = c(0,100,200,300,400,500,600,700,800),
        labels = c("-4kb","-3kb","-2kb","-1kb","0","1kb","2kb","3kb","4kb")
      ) +
      ggplot2::theme(legend.position = "none")

    # return plot
    return(g1)
  }
}

#'
#' Function to build the x scale based on nother of bins
#'
scale_x_bins <- function(data) {
  bins <- table(ggplot2::cut_number(data[["average"]]$bin,8, labels = FALSE))
  breaks <- as.numeric(c(1, cumsum(bins)))
  # start from bin_size and get the central referencePoint
  central_bin <- data[["stats"]]$central_bin
  labels <- lapply(breaks, function(b) {
    # put 0 on central bin
    if (b == central_bin) {
      0
    } else {
      
    }
  })
  list(
    breaks = breaks,
    labels = labels
  )
}