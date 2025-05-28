#' Select Bins
#' @description Selects the columns that represent bins in a data frame.
#' @param data A data frame containing genomic data with bin columns.
#' @return A data frame with only the bin columns.
#' 
selectBins <- function(data) {
  data |> dplyr::select(!(file:strand))
}
