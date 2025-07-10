#' Parse a samplesheet file
#'
#' This function is intended to parse a samplesheet file,
#' which typically contains metadata about samples
#' used in an experiment.
#'
#' @param file A character string specifying the path to the samplesheet file.
#' @param root_path A character string specifying the root
#' path for the samplesheet.
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
#' parse_samplesheet(example_samplesheet, getwd())
#'
#' @export
parse_samplesheet <- function(
  file,
  root_path,
  timepoint_levels = c("BL", "BR", "PD")
) {

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

  # mutate table into samples
  samples <- t |>
    dplyr::mutate(timepoint = factor(timepoint, levels = timepoint_levels)) |>
    dplyr::mutate(
      encoded_timepoint = as.integer(timepoint),
      .after = "timepoint"
    ) |>
    dplyr::select(caseid, sampleid, timepoint, encoded_timepoint)

  # add root path column for results
  datapaths <- unlist(samples |> purrr::pmap(function(caseid, sampleid, timepoint, encoded_timepoint){
    file.path(root_path, caseid, sampleid, "fragmentomics/processed/matrix")
  }))

  samples <- samples |>
    dplyr::mutate(datapath=datapaths)

  samples
}
