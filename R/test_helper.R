#'
#' Helper function to build a directory structure for testing
#' you can load in env source(<file>)
#'
#' @param root the root directory to build the structure dirs
#'
build_dir_structure <- function(root = getwd()) {fragdir <- file.path(root, "results")
  dir.create(fragdir, showWarnings = FALSE)

  # build directory structure for samplesheet.csv
  samplesheet <- system.file("extdata",
                             "samplesheet.csv",
                             package = "fragmentomics")

  samples <- fragmentomics::parse_samplesheet(samplesheet)

  # CTCF and ELK4 example file
  ctcf_example_file <- system.file("extdata",
                                   "CTCF.gz",
                                   package = "fragmentomics")

  elk4_example_file <- system.file("extdata",
                                   "ELK4.gz",
                                   package = "fragmentomics")

  ctcf_stats_file <- system.file("extdata",
                                 "CTCF_peak_stats.tsv",
                                 package = "fragmentomics")

  elk4_stats_file <- system.file("extdata",
                                 "ELK4_peak_stats.tsv",
                                 package = "fragmentomics")

  ctcf_data_file <- system.file("extdata",
                                 "CTCF_peak_data.tsv",
                                 package = "fragmentomics")

  elk4_data_file <- system.file("extdata",
                                 "ELK4_peak_data.tsv",
                                 package = "fragmentomics")

  samples |> purrr::pmap(function(caseid,
                                  sampleid,
                                  timepoint,
                                  encoded_timepoint) {

    # MATRIX
    mpath <- file.path(fragdir,
                       caseid,
                       sampleid,
                       "fragmentomics",
                       "processed",
                       "matrix",
                       "source_1",
                       "CTCF")

    dir.create(mpath, showWarnings = FALSE, recursive = TRUE)
    file.copy(ctcf_example_file, mpath, overwrite = TRUE)

    # STATS AND DATA
    mpath <- file.path(fragdir,
                       caseid,
                       sampleid,
                       "fragmentomics",
                       "processed",
                       "peakstats",
                       "source_1",
                       "CTCF")

    dir.create(mpath, showWarnings = FALSE, recursive = TRUE)
    file.copy(ctcf_stats_file, mpath, overwrite = TRUE)
    file.copy(ctcf_data_file, mpath, overwrite = TRUE)

    # MATRIX
    mpath <- file.path(fragdir,
                       caseid,
                       sampleid,
                       "fragmentomics",
                       "processed",
                       "matrix",
                       "source_1",
                       "ELK4")

    dir.create(mpath, showWarnings = FALSE, recursive = TRUE)
    file.copy(elk4_example_file, mpath, overwrite = TRUE)

    # STATS AND DATA
    mpath <- file.path(fragdir,
                       caseid,
                       sampleid,
                       "fragmentomics",
                       "processed",
                       "peakstats",
                       "source_1",
                       "ELK4")

    dir.create(mpath, showWarnings = FALSE, recursive = TRUE)
    file.copy(elk4_stats_file, mpath, overwrite = TRUE)
    file.copy(elk4_data_file, mpath, overwrite = TRUE)

  })

  fragdir
}
