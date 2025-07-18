% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/peak_plot.R
\name{peak_plot}
\alias{peak_plot}
\title{Peak Plot}
\usage{
peak_plot(data, normalized = FALSE, palette = "Set1")
}
\arguments{
\item{data}{A data frame containing the data to be plotted.}

\item{normalized}{A logical value indicating whether the data is normalized.
If TRUE, the plot will show relative signal;
if FALSE, it will show raw signal.}

\item{palette}{A character string representing the
color palette to be used for the plot.}
}
\value{
A ggplot object.
}
\description{
This function generates a ggplot plot for the given data.
}
\examples{
file <- system.file("extdata", "CTCF_matrix.gz", package = "fragmentomics")
matrix_data <- parse_compute_matrix(file)
peak_data <- peak_stats(matrix_data)
peak_plot(peak_data)
peak_plot(peak_data, normalized=TRUE)

}
