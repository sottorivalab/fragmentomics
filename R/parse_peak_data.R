#' Parse peak data
#'
#'
#' This function reads peak data from a file and returns it as a tibble.
#'
#' @param file A character string specifying the path to the peak data file.
#'
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
