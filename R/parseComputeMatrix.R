#' Parse Compute Matrix
#'
#' This function parse output from deeptools [computeMatrix](https://deeptools.readthedocs.io/en/develop/content/tools/computeMatrix.html)
#'
#' @param file Path to the computeMatrix output file.
#' @returns A tibble containing the parsed data.
#'  1. columns 1-23 are metadata columns
#'  2. columns 24-29 are chr, start, end, name, score, strand
#'  3. columns 30+ are bins with values
#'
parseComputeMatrix <- function(file) {

  # Check if the file exists
  if (!file.exists(file)) {
    stop("File does not exist: ", file)
  }

  # Read the file, skip first line (is json data)
  data <- tibble::as_tibble(utils::read.delim(file, header = FALSE, skip = 1))

  # Check if the data is empty
  if (nrow(data) == 0) {
    stop("The file is empty or does not contain valid data.")
  }

  # get headers
  headers <- jsonlite::fromJSON(stringr::str_remove(readr::read_lines(file, n_max = 1), "@"))

  # rename position columns
  lookup <- c(
    chr = "V1",
    start = "V2",
    end = "V3",
    name = "V4",
    score = "V5",
    strand = "V6"
  )

  # Check if the first six columns are present
  if (ncol(data) < 6) {
    stop("The file does not contain enough columns for chromosome, start, end, name, score, and strand.")
  }

  bins <- colnames(data)[7:length(colnames(data))]
  for (i in 1:length(bins)) {
    source = bins[i]
    target = paste("bin_",i, sep="")
    lookup[target] = source
  }

  data <- dplyr::rename(data, dplyr::all_of(lookup))

  # Remove NaN
  data <- data |> dplyr::mutate_all(function(x) ifelse(is.nan(x), NA, x))

  # add headers info
  data <- data |> dplyr::mutate(
    file = file,
    upstream = headers$upstream,
    downstream = headers$downstream,
    body = headers$body,
    bin_size = headers[['bin size']],
    ref_point = headers[['ref point']],
    verbose = headers$verbose,
    bin_avg_type = headers[['bin avg type']],
    missing_data_as_zero = headers[['missing data as zero']],
    min_threshold = headers[['min threshold']],
    max_threshold = headers[['max threshold']],
    scale = headers$scale,
    skip_zeros = headers[['skip zeros']],
    nan_after_end = headers[['nan after end']],
    proc_number = headers[['proc number']],
    sort_regions = headers[['sort regions']],
    sort_using = headers[['sort using']],
    unscaled_5_prime = headers[['unscaled 5 prime']],
    unscaled_3_prime = headers[['unscaled 3 prime']],
    sample_labels = headers[['sample_labels']],
    group_labels = headers[['group_labels']],
    sample_boundaries_start = headers[['sample_boundaries']][1],
    sample_boundaries_end = headers[['sample_boundaries']][2],
    group_boundaries_start = headers[['group_boundaries']][1],
    group_boundaries_end = headers[['group_boundaries']][2],
    .before = "chr"
  )

  # Return the parsed data
  return(data)
}
