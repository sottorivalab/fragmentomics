#' Parse a samplesheet file
#'
#' This function is intended to parse a samplesheet file,
#' which typically contains metadata about samples
#' used in an experiment.
#'
#' @param file A character string specifying the path to the samplesheet file.
#' @param timepoint_levels A character vector specifying the levels for the
#' timepoint factor.
#'
#' @returns A tibble containing the parsed samplesheet data.
#'
#' @examples
#' example_samplesheet <- system.file(
#'   "extdata",
#'   "samplesheet.csv",
#'   package = "fragmentomics"
#' )
#' parse_samplesheet(example_samplesheet)
#'
#' @export
parse_samplesheet <- function(file, timepoint_levels = c("BL", "BR", "PD")) {
  t <- readr::read_csv(file, show_col_types = FALSE)
  t |>
    dplyr::mutate(timepoint = factor(timepoint, levels = timepoint_levels)) |>
    dplyr::mutate(
      encoded_timepoint = as.integer(timepoint),
      .after = "timepoint"
    )
}
