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

  # Check if the file exists
  if (!file.exists(file)) {
    stop("File does not exist: ", file)
  }

  t <- readr::read_csv(file, show_col_types = FALSE)

  # check timepoints in samplesheet
  if (!all(t$timepoint %in% timepoint_levels)) {
    stop(
      "Not all timepoints in samplesheet are in the expected levels: ",
      paste(timepoint_levels, collapse = ", ")
    )
  }

  t |>
    dplyr::mutate(timepoint = factor(timepoint, levels = timepoint_levels)) |>
    dplyr::mutate(
      encoded_timepoint = as.integer(timepoint),
      .after = "timepoint"
    ) |>
    dplyr::select(caseid, sampleid, timepoint, encoded_timepoint)
}
