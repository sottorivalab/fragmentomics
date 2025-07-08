#' Parse a samplesheet file
#'
#' This function is intended to parse a samplesheet file,
#' which typically contains metadata about samples
#' used in an experiment.
#'
#' @param file A character string specifying the path to the samplesheet file.
#'
#' @export
parse_samplesheet <- function(file) {
  readr::read_csv(file, show_col_types = FALSE)
}
