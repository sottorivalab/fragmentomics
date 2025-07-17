#'
#' Parse peak stats from a file
#'
#' @param file Path to the file containing peak stats.
#'
#' @return A tibble with the parsed peak stats.
#'
#' @examples
#' file <- system.file("extdata",
#'   "CTCF_peak_stats.tsv",
#'   package = "fragmentomics"
#' )
#' parse_peak_stats(file)
#' @export
parse_peak_stats <- function(file) {
  readr::read_tsv(
    file,
    show_col_types = FALSE,
    col_types = readr::cols(
      signal_label = readr::col_character(),
      target_label = readr::col_character(),
      source_label = readr::col_character(),
      integration = readr::col_double(),
      background_mean = readr::col_double(),
      background_left_limit = readr::col_double(),
      background_right_limit = readr::col_double(),
      central_bin = readr::col_integer(),
      bin_size = readr::col_integer(),
      reference_point_coverage = readr::col_double(),
      reference_point_relative = readr::col_double(),
      central_coverage = readr::col_double(),
      central_coverage_bin_min = readr::col_double(),
      central_coverage_bin_max = readr::col_double(),
      average_coverage = readr::col_double(),
      average_coverage_bin_min = readr::col_double(),
      average_coverage_bin_max = readr::col_double(),
      peak_length = readr::col_double(),
      peak_relative = readr::col_double()
    )
  )
}
