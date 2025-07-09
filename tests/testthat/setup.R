tmpdir <- tempdir(check = TRUE)
fragdir <- file.path(tmpdir,"results")
#print(paste("Setting up tmp directory", fragdir, sep=" "))

dir.create(fragdir, showWarnings = FALSE)

# check if dir was created
if (!dir.exists(fragdir)) {
  stop("Temporary directory for results does not exist: ", fragdir)
}

# build directory structure for samplesheet.csv
samplesheet <- system.file("extdata", "samplesheet.csv", package = "fragmentomics")
samples <- fragmentomics::parse_samplesheet(samplesheet)

# CTCF and ELK4 example file
ctcf_example_file <- system.file("extdata", "CTCF.gz", package = "fragmentomics")
elk4_example_file <- system.file("extdata", "ELK4.gz", package = "fragmentomics")

samples |> purrr::pmap(function(caseid, sampleid, timepoint, encoded_timepoint) {
  mpath <- file.path(fragdir, caseid, sampleid, "fragmentomics", "processed", "matrix", "source_1","CTCF")
  dir.create(mpath, showWarnings = FALSE, recursive = TRUE)
  file.copy(ctcf_example_file, mpath, overwrite = TRUE )
  mpath <- file.path(fragdir, caseid, sampleid, "fragmentomics", "processed", "matrix", "source_1","ELK4")
  dir.create(mpath, showWarnings = FALSE, recursive = TRUE)
  file.copy(elk4_example_file, mpath, overwrite = TRUE )
})

fs::dir_tree(fragdir)

withr::defer(fs::dir_delete(fragdir), teardown_env())
