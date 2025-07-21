#' Zscore for housekeeping genes
#'
#' This function calculates the z-score for housekeeping genes compared to random datasets.
#'
#' @param housekeeping A tibble containing the housekeeping gene stats data.
#' @param random A tibble containing the random datasets stats data.
#' @return A tibble containing the z-scores for the housekeeping genes.
#' @importFrom grDevices nclass.Sturges
#' @importFrom stats pnorm
#' @importFrom stats sd
#' @export
housekeeping_zscore <- function(housekeeping, random) {
  hp <- housekeeping$peak_length
  rp <- random$peak_length
  z <- (hp - mean(rp)) / sd(rp)
  p <- pnorm(q=z, lower.tail = FALSE)

  tibble::tibble(
    z = z,
    p = p
  )
}
