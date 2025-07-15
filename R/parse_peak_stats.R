#'
#' Parse peak stats from a file
#'
#' @param file Path to the file containing peak stats.
#'
#' @return A tibble with the parsed peak stats.
#'
#' @examples
#' file <- system.file("extdata", "CTCF_peak_stats.tsv", package = "fragmentomics")
#' parse_peak_stats(file)
#' @export
parse_peak_stats <- function(file) {
  readr::read_tsv(file, show_col_types = FALSE)
}
