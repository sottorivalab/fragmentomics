#' Parse peak data
#' @export
parse_peak_data <- function(file) {
  readr::read_tsv(
    file,
    show_col_types = FALSE,
    col_types = readr::cols(
      bin = readr::col_integer(),
      coverage = readr::col_double(),
      relative = readr::col_double(),
      background.mean = readr::col_double()
    )
  )
}
