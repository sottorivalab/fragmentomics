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
      signal = readr::col_character(),
      target = readr::col_character(),
      source = readr::col_character(),
      integration = readr::col_double(),
      background.mean = readr::col_double(),
      referencePoint.bin = readr::col_integer(),
      referencePoint.coverage = readr::col_double(),
      referencePoint.relative = readr::col_double(),
      central.coverage = readr::col_double(),
      central.coverage.bin.min = readr::col_double(),
      central.coverage.bin.max = readr::col_double(),
      background.left.limit = readr::col_double(),
      background.right.limit = readr::col_double(),
      average.coverage = readr::col_double(),
      average.coverage.bin.min = readr::col_double(),
      average.coverage.bin.max = readr::col_double(),
      peak.length = readr::col_double(),
      peak.relative.length = readr::col_double()
    )
  )
}
