#'
#' Calculate background mean using the limits of the distribution.
#' Aka the extreme parts of the peak that will represent the background
#'
#' @param data A tibble with the coverage data
#' @param signal_label A label for the signal, defaults to sample_labels
#' @param target_label A label for the target, defaults to file
#' @param source_label A label for the source, defaults to NA
#' @param left The number of bins to the left of the peak to use for background calculation, defaults to 50
#' @param right The number of bins to the right of the peak to use for background calculation, defaults to 50
#' @param central The number of bins around the central point to use for peak calculation, defaults to 30
#' @param average The number of bins around the central point to use for average calculation, defaults to 1000
#' @param rpoints The number of random points to use for Monte Carlo integration, defaults to 10000
#' @return A list containing the peak statistics, integration data, and average data
#'
#' @export

peakStats <- function(data, signal_label=NULL, target_label=NULL, source_label=NULL, left=50, right=50, central=30, average=1000, rpoints=10000) {

  # if not signal label use sample_labels
  if (is.null(signal_label)) {
    signal_label <- as.character(data |> dplyr::select(sample_labels) |> unique())
  }

  # if not target_labels use file
  if (is.null(target_label)) {
    target_label <- as.character(data |> dplyr::select(file) |> unique())
  }

  # if not source_label use NA
  if (is.null(source_label)) {
    source_label <- NA
  }

  # average data
  bins <- data |> dplyr::select(!(file:strand))
  average.data <- bins |>
    dplyr::summarise(dplyr::across(dplyr::everything(), \(x) mean(x, na.rm = TRUE))) |>
    tidyr::pivot_longer(colnames(bins)) |>
    dplyr::rename(bin="name", coverage="value") |>
    dplyr::mutate(name = bin, .before=bin) |>
    dplyr::mutate(bin = dplyr::row_number(), .after=name)

  # calculate the extremes where left is X and right is L-Y
  background.left.limit <- left
  background.right.limit <- nrow(average.data) - right
  background.data <- average.data |>
    dplyr::filter(bin <= background.left.limit | bin >= background.right.limit)
  background.mean <- mean(background.data$coverage)

  # add relative signal
  average.data <- average.data |>
    dplyr::mutate(relative = coverage/mean(coverage)) |>
    dplyr::mutate(mean = mean(coverage))

  # central point
  central.bin <- round(max(average.data$bin)/2, digits=0)
  reference.point <- average.data |>
    dplyr::filter(bin == central.bin)

  # central coverage
  bin.size <- as.numeric(data |> dplyr::select(bin_size) |> unique())
  central.coverage.bin.min <- central.bin - (central / bin.size)
  central.coverage.bin.max <- central.bin + (central / bin.size)
  central.coverage.data <- average.data |>
    dplyr::filter(bin >= central.coverage.bin.min, bin <= central.coverage.bin.max) |>
    dplyr::select(coverage)
  central.coverage <- mean(central.coverage.data$coverage)

  # average coverage
  average.coverage.bin.min <- central.bin - (average / bin.size)
  average.coverage.bin.max <- central.bin + (average / bin.size)

  average.coverage.data <- average.data |>
    dplyr::filter(bin >= average.coverage.bin.min, bin <= average.coverage.bin.max) |>
    dplyr::select(coverage)

  average.coverage <- mean(average.coverage.data$coverage)

  # monte carlo integration
  x1 <- stats::runif(rpoints, min=1, max=nrow(average.data))
  y1 <- stats::runif(rpoints, min=min(average.data$coverage) , max=max(average.data$coverage))
  random.points <- tibble::tibble(x=x1,y=y1) |> dplyr::arrange(x)

  # left join, this expand the data to N=random.points, annotate the points above and below
  integration.data <- random.points |>
    dplyr::left_join(average.data, dplyr::join_by(closest(x >= bin))) |>
    dplyr::mutate(
      above=(y >= coverage & y <= background.mean),
      background.mean=background.mean
    )

  # monte carlo integration
  montecarlo.integration <- length(which(integration.data$above)) / nrow(integration.data)

  # peak length
  peak.length <- background.mean - reference.point$coverage
  peak.relative <- 1 - reference.point$relative

  # update table
  peak.stats <- tibble::tibble(
      signal_label = signal_label,
      target_label = target_label,
      source_label = source_label,
      integration = montecarlo.integration,
      background_mean = background.mean,
      background_left_limit = background.left.limit,
      background_right_limit = background.right.limit,
      central_bin = central.bin,
      bin_size = bin.size,
      reference_point_coverage = reference.point$coverage,
      reference_point_relative = reference.point$relative,
      central_coverage = central.coverage,
      central_coverage_bin_min = central.coverage.bin.min,
      central_coverage_bin_max = central.coverage.bin.max,
      average_coverage = average.coverage,
      average_coverage_bin_min = average.coverage.bin.min,
      average_coverage_bin_max = average.coverage.bin.max,
      peak_length = peak.length,
      peak_relative = peak.relative
    )

  return(
    list(
      stats = peak.stats,
      integration = integration.data,
      average = average.data
    )
  )
}
