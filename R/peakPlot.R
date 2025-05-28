#' Peak Plot
#'
#' This function generates a ggplot plot for the given data.
#'
#' @param data A data frame containing the data to be plotted.
#' @return A ggplot object.

peakPlot <- function(data) {
  # labels
  label.pos <- -(max(data[['average']]$bin) * .1)
  # peak length
  peak.length <- tibble(
    x=max(data[['average']]$bin),
    y=c(
      data[['stats']]$reference_point_coverage,
      data[['stats']]$background_mean
    )
  )
  # plot
  # plot raw signal
  g1 <- ggplot() +
    # composite coverage
    geom_line(data=data[['average']],aes(y=coverage, x=bin)) +
    # integration data
    geom_point(data=data[['integration']],aes(x = x, y = y, color=above), size = .2, alpha = .5) +
    scale_color_manual(values=c("#D3D3D3", "#56B4E9")) +
    # background mean
    geom_hline(yintercept = data[['stats']]$background_mean, color="#6082B6", linetype = 'dotted') +
    geom_text(aes(label.pos,data[['stats']]$background_mean,label = "background"), color="#6082B6", size = 2.5, vjust = -1) +
    geom_rect(aes(xmin = 0,xmax = data[['stats']]$background_left_limit,  ymin=min(data[['average']]$coverage),ymax=max(data[['average']]$coverage)),
      color="#6082B6",
      alpha = .15
    ) +
    geom_rect(
      aes(
        xmin = data[['stats']]$background_right_limit,
        xmax = max(data[['average']]$bin),
        ymin=min(data[['average']]$coverage),
        ymax=max(data[['average']]$coverage)
      ),
      color="#6082B6",
      alpha = .15
    ) +
    # reference coverage
    geom_hline(yintercept = data[['stats']]$reference_point_coverage, color="#6082B6", linetype = 'dotted') +
    geom_text(aes(label.pos,data[['stats']]$reference_point_coverage,label = "reference"), size = 2.5, vjust = 1, color="#6082B6") +
    # central coverage
    geom_hline(yintercept = data[['stats']]$central_coverage, color="orange", linetype = 'dotted') +
    geom_text(aes(label.pos,data[['stats']]$central_coverage,label = "central"), size = 2.5, vjust = -1, color="orange") +
    # central coverage limits
    geom_rect(
      aes(
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
    geom_hline(yintercept = data[['stats']]$average_coverage, color="red", linetype = 'dotted') +
    geom_text(aes(label.pos,data[['stats']]$average_coverage,label = "average"), size = 2.5, vjust = -1, color="red") +
    # average coverage limits
    geom_rect(
      aes(
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
    geom_point(data=peak.length, aes(x=x, y=y), color="green", size=1) +
    geom_line(data=peak.length, aes(x=x, y=y), color="green", linetype="dashed") +
    # labels
    xlab(data[['stats']]$target_label) +
    ylab(data[['stats']]$signal_label) +
    ggtitle(
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
    scale_x_continuous(
      "Position relative to referencePoint (bp)",
      breaks = c(0,100,200,300,400,500,600,700,800),
      labels = c("-4kb","-3kb","-2kb","-1kb","0","1kb","2kb","3kb","4kb")
    ) +
    theme(legend.position = "none")
  
  # return plot
  return(g1)
}


