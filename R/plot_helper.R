#'
#' scale_x_bins
#'
#' Function to build the x scale
#'
#' @param bin_vector A numeric vector representing the bins.
#' @param central_bin An integer representing the central bin.
#' @param bin_size An integer representing the size of the bins.
#'
#' @return A list with breaks and labels for the x scale.
scale_x_bins <- function(bin_vector, central_bin, bin_size) {
  bins <- table(ggplot2::cut_number(bin_vector, 8, labels = FALSE))
  breaks <- as.numeric(c(0, cumsum(bins)))
  # the central referencePoint
  labels <- lapply(breaks, function(br) {
    (br - central_bin) * bin_size
  })
  labels <- lapply(labels, label_basepairs)
  list(
    breaks = breaks,
    labels = unlist(labels)
  )
}

#'
#' Label basepairs
#'
#' @param x A numeric vector representing basepairs.
#' @return A character vector with basepairs labeled with appropriate suffixes.
#'
label_basepairs <- function(x) {
  breaks <- c(0, 10^c("k" = 3, "M" = 6, "G" = 9, "T" = 12))
  n_suffix <- cut(abs(x),
                  breaks = c(unname(breaks), Inf),
                  labels = c(names(breaks)),
                  right = FALSE)
  n_suffix[is.na(n_suffix)] <- ""
  # Don't suffix basepairs unless >= kb
  suffix <- ifelse(nzchar(as.character(n_suffix)),
                   paste0("", n_suffix, "b"), "")
  scale <- 1 / breaks[n_suffix]
  scale[which(scale %in% c(Inf, NA))] <- 1
  paste(x * scale, suffix, sep = "")
}

#'
#' custom_legend
#' Function to create a custom legend for ggplot2.
#' NOTE: the labels are reorder in alphabetical order.
#'
#' @param labels A character vector of labels for the legend.
#' @param ... A variable number of aesthetic mappings to be used in the legend.
#' @param title A character string for the title of the legend.
#' @param key A function to draw the key glyph
#' in the legend (default is `draw_key_point`).
#' @param guide_args A list of additional arguments
#' to be passed to the `guide_legend` function.
#' @return A list containing a dummy geom and a dummy scale for the legend.
custom_legend <- function(labels = NULL,
                          ...,
                          title = NULL,
                          key   = ggplot2::draw_key_point,
                          guide_args = list()) {
  # Capture arguments
  aesthetics <- list(...)
  n <- max(lengths(aesthetics), 0)
  labels <- labels %||%  seq_len(n)

  # Overrule the alpha = 0 that we use to hide the points
  aesthetics$alpha <- aesthetics$alpha %||% rep(1, n)

  # Construct guide
  guide_args$override.aes <- guide_args$override.aes %||% aesthetics
  guide <- do.call(ggplot2::guide_legend, guide_args)

  # Allow dummy aesthetic
  ggplot2::update_geom_defaults("point", list(dummy = "x"))

  dummy_geom <- ggplot2::geom_point(
    data = data.frame(x = rep(Inf, n), y = rep(Inf, n),
                      dummy = factor(labels)),
    ggplot2::aes(x, y, dummy = dummy), alpha = 0, key_glyph = key
  )

  dummy_scale <- ggplot2::discrete_scale(
    "dummy", palette = scales::identity_pal(), name = title,
    guide = guide
  )

  list(dummy_geom, dummy_scale)
}



heatmap_color <- function(matrix_data) {
  # Get limits for an unified legend
  w <- purrr::list_c(lapply(matrix_data, function(mdata){
    as.vector(mdata)
  }))

  # heatmap color ramp
  heatmap_limits <- base::append(stats::quantile(w, probs = c(0.01, 0.99)),
                                  0,
                                  after = 1)

  heatmap_colorfun <- circlize::colorRamp2(heatmap_limits,
                                           c("red", "white", "blue"))
  heatmap_colorfun
}
