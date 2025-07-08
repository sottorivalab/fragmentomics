#'
#' Calculate background mean using the limits of the distribution.
#' Aka the extreme parts of the peak that will represent the background
#'
#' @param data A tibble with the coverage data
#' @param signal_label A label for the signal, defaults to sample_labels
#' @param target_label A label for the target, defaults to file
#' @param source_label A label for the source, defaults to NA
#' @param left The number of bins to the left of the peak
#' to use for background calculation, defaults to 50
#' @param right The number of bins to the right of the peak
#' to use for background calculation, defaults to 50
#' @param central The number of bins around the central point
#' to use for peak calculation, defaults to 30
#' @param average The number of bins around the central point
#' to use for average calculation, defaults to 1000
#' @param rpoints The number of random points to use for
#' Monte Carlo integration, defaults to 10000
#' @return A list containing the peak statistics,
#' integration data, and average data
#'
#' @export

peak_stats <- function(
    data,
    signal_label = NULL,
    target_label = NULL,
    source_label = NULL,
    left = 50,
    right = 50,
    central = 30,
    average = 1000,
    rpoints = 10000) {
  # if not signal label use sample_labels
  if (is.null(signal_label)) {
    signal_label <- as.character(
      data |> dplyr::select(sample_labels) |> unique()
    )
  }

  # if not target_labels use file removing path and extension
  if (is.null(target_label)) {
    target_label <- fs::path_file(
      fs::path_ext_remove(
        as.character(data |> dplyr::select(file) |> unique())
      )
    )
  }

  # if not source_label use NA
  if (is.null(source_label)) {
    source_label <- NA
  }

  # average data
  bins <- data |> dplyr::select(!(file:strand))
  average_data <- bins |>
    dplyr::summarise(
      dplyr::across(dplyr::everything(), \(x) mean(x, na.rm = TRUE))
    ) |>
    tidyr::pivot_longer(colnames(bins)) |>
    dplyr::rename(bin = "name", coverage = "value") |>
    dplyr::mutate(name = bin, .before = bin) |>
    dplyr::mutate(bin = dplyr::row_number(), .after = name)

  # calculate the extremes where left is X and right is L-Y
  background_left_limit <- left
  background_right_limit <- nrow(average_data) - right
  background_data <- average_data |>
    dplyr::filter(bin <= background_left_limit | bin >= background_right_limit)
  background_mean <- mean(background_data$coverage)

  # add relative signal
  average_data <- average_data |>
    dplyr::mutate(relative = coverage / mean(coverage)) |>
    dplyr::mutate(mean = mean(coverage))

  # central point
  central_bin <- round(max(average_data$bin) / 2, digits = 0)
  reference_point <- average_data |>
    dplyr::filter(bin == central_bin)

  # central coverage
  bin_size <- as.numeric(data |> dplyr::select(bin_size) |> unique())
  central_coverage_bin_min <- central_bin - (central / bin_size)
  central_coverage_bin_max <- central_bin + (central / bin_size)
  central_coverage_data <- average_data |>
    dplyr::filter(
      bin >= central_coverage_bin_min, bin <= central_coverage_bin_max
    ) |>
    dplyr::select(coverage)
  central_coverage <- mean(central_coverage_data$coverage)

  # average coverage
  average_coverage_bin_min <- central_bin - (average / bin_size)
  average_coverage_bin_max <- central_bin + (average / bin_size)

  average_coverage_data <- average_data |>
    dplyr::filter(
      bin >= average_coverage_bin_min,
      bin <= average_coverage_bin_max
    ) |>
    dplyr::select(coverage)

  average_coverage <- mean(average_coverage_data$coverage)

  # monte carlo integration
  x1 <- stats::runif(rpoints, min = 1, max = nrow(average_data))
  y1 <- stats::runif(
    rpoints, min = min(average_data$coverage), max = max(average_data$coverage)
  )
  random_points <- tibble::tibble(x = x1, y = y1) |> dplyr::arrange(x)

  # left join, this expand the data to N=random.points,
  # annotate the points above and below
  integration_data <- random_points |>
    dplyr::left_join(average_data, dplyr::join_by(closest(x >= bin))) |>
    dplyr::mutate(
      above = (y >= coverage & y <= background_mean),
      background.mean = background_mean
    )

  # monte carlo integration
  montecarlo_integration <- (
    length(which(integration_data$above)) / nrow(integration_data)
  )

  # peak length
  peak_length <- background_mean - reference_point$coverage
  peak_relative <- 1 - reference_point$relative

  # update table
  peak_stats <- tibble::tibble(
    signal_label = signal_label,
    target_label = target_label,
    source_label = source_label,
    integration = montecarlo_integration,
    background_mean = background_mean,
    background_left_limit = background_left_limit,
    background_right_limit = background_right_limit,
    central_bin = central_bin,
    bin_size = bin_size,
    reference_point_coverage = reference_point$coverage,
    reference_point_relative = reference_point$relative,
    central_coverage = central_coverage,
    central_coverage_bin_min = central_coverage_bin_min,
    central_coverage_bin_max = central_coverage_bin_max,
    average_coverage = average_coverage,
    average_coverage_bin_min = average_coverage_bin_min,
    average_coverage_bin_max = average_coverage_bin_max,
    peak_length = peak_length,
    peak_relative = peak_relative
  )

  return(
    list(
      stats = peak_stats,
      integration = integration_data,
      average = average_data
    )
  )
}
