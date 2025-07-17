#' Parse peak data
#'
#'
#' This function reads peak data from a file and returns it as a tibble.
#'
#' @param file A character string specifying the path to the peak data file.
#'
#' @return A tibble containing the parsed peak data.
#'
#' @examples
#' file <- system.file("extdata",
#'                     "CTCF_peak_data.tsv",
#'                     package = "fragmentomics")
#' peak_data <- parse_peak_data(file)
#'
#' @export
parse_peak_data <- function(file) {
  mcols <- readr::cols(bin = readr::col_integer(),
                       coverage = readr::col_double(),
                       relative = readr::col_double(),
                       mean = readr::col_double())

  readr::read_tsv(file, show_col_types = FALSE, col_types = mcols)
}
