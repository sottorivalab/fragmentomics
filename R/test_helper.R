#'
#' Helper function to build a directory structure for testing
#' it use files in inst/extdata
#'
#' @param root the root directory to build the structure dirs
#'
build_dir_structure <- function(root = getwd()) {

  # results dir
  fragdir <- file.path(root, "results")
  dir.create(fragdir, showWarnings = FALSE)

  # load samplesheet.csv
  samplesheet <- system.file("extdata",
                             "samplesheet.csv",
                             package = "fragmentomics")

  samples <- fragmentomics::parse_samplesheet(samplesheet)

  targets <- list(
    source_1 = c("CTCF","ELK4"),
    house_keeping_dataset = c("GeneHancer_housekeeping"),
    random_dataset = c("rand1", "rand2")
  )

  samples |> purrr::pmap(function(caseid,
                                  sampleid,
                                  timepoint,
                                  encoded_timepoint) {

    sampledir <- file.path(fragdir, caseid, sampleid)
    dir.create(sampledir, showWarnings = FALSE, recursive = TRUE)
    pdir <- file.path(sampledir, "fragmentomics","processed")
    dir.create(pdir, showWarnings = FALSE, recursive = TRUE)
    mdir <- file.path(pdir, "matrix")
    dir.create(mdir, showWarnings = FALSE, recursive = TRUE)
    sdir <- file.path(pdir, "peakstats")
    dir.create(sdir, showWarnings = FALSE, recursive = TRUE)
    lapply(names(targets), function(source) {
      sourcedir_peakstats <- file.path(sdir, source)
      dir.create(sourcedir_peakstats, showWarnings = FALSE, recursive = TRUE)
      sourcedir_matrix <- file.path(mdir, source)
      dir.create(sourcedir_matrix, showWarnings = FALSE, recursive = TRUE)
      mt <- targets[[source]]
      lapply(mt, function(target) {
        # matrix files
        matrixdir <- file.path(sourcedir_matrix, target)
        dir.create(matrixdir, showWarnings = FALSE, recursive = TRUE)
        mname <- paste(target,".gz",sep="")
        mpath <- system.file("extdata", mname, package = "fragmentomics")
        mdest <- file.path(matrixdir, mname)
        file.copy(mpath, mdest, overwrite = TRUE)

        # peak stats
        psdir <- file.path(sourcedir_peakstats, target)
        dir.create(psdir, showWarnings = FALSE, recursive = TRUE)
        sname <- paste(target,"_peak_stats.tsv",sep="")
        spath <- system.file("extdata", sname, package = "fragmentomics")
        sdest <- file.path(psdir, sname)
        file.copy(spath, sdest, overwrite = TRUE)

        # peak data
        dname <- paste(target,"_peak_data.tsv",sep="")
        dpath <- system.file("extdata", sname, package = "fragmentomics")
        ddest <- file.path(psdir, dname)
        file.copy(dpath, ddest, overwrite = TRUE)
      })
    })
  })

  fragdir
}

