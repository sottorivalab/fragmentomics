#' Cohort heatmap
#' This function creates a heatmap for the cohort data in a list of matrices
#'
#' @param matrix_data A list of matrices containing the cohort data.
#' @param signal A character string specifying the
#' signal to use for the heatmap.
#' @param figure_title A character string specifying
#' the title of the list of heatmaps.
#'
#' @returns A heatmap object.
#' @export
cohort_heatmap <- function(matrix_data,
                           signal = "central_coverage",
                           figure_title = "Cohort heatmap") {

  figure_title_opt <- grid::gpar(fontsize = 14)
  heatmap_width <- ggplot2::unit(4, "cm")
  rownames_opt <- grid::gpar(fontsize = 4)
  colnames_opt <- grid::gpar(fontsize = 4)

  heatmap_colorfun <- heatmap_color(matrix_data)
  heatmap_legend <- ComplexHeatmap::Legend(col_fun = heatmap_colorfun,
                                           title = signal)

  legend_list <- list(heatmap_legend)

  heatmaps <- lapply(names(matrix_data), function(mname) {
    mdata <- matrix_data[[mname]]
    h <- ComplexHeatmap::Heatmap(mdata,
                                 name = mname,
                                 column_title = mname,
                                 show_row_names = TRUE,
                                 width = heatmap_width,
                                 show_heatmap_legend = FALSE,
                                 row_names_gp = rownames_opt,
                                 column_names_gp = colnames_opt,
                                 col = heatmap_colorfun)
    h
  })

  # build a list
  ht_list <- NULL
  for (h in heatmaps) {
    ht_list <- ht_list + h
  }

  ComplexHeatmap::draw(ht_list,
                       annotation_legend_list = legend_list,
                       heatmap_legend_side = "bottom",
                       column_title = figure_title,
                       column_title_gp = figure_title_opt)
}
