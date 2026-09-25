# The unzipped files of a fake fit written by hand in a test: `<file>-*` plus
# `<file>.csv` and `<file>.R`, matched literally.  saveFit() itself no longer
# picks files by name (it writes them in a private directory), since `-` is
# legal in a base name and `fit-*` also matches the files of `fit-alt`.
.nlmixr2saveFitFiles <- function(file) {
  .base <- basename(file)
  .dir <- dirname(file)
  # dirname("fit") is "." but dirname("") is "", and file.path("", x) would
  # make that an absolute path at the filesystem root
  if (.dir == "") {
    .dir <- "."
  }
  # all.files: a base name can start with a dot, since `.fit` is an ordinary
  # R name and saveFit() takes the base name from the variable
  .all <- setdiff(list.files(.dir, all.files = TRUE), c(".", ".."))
  .keep <- startsWith(.all, .base) &
    (substring(.all, nchar(.base) + 1L, nchar(.base) + 1L) == "-" |
      .all == paste0(.base, ".csv") |
      .all == paste0(.base, ".R"))
  gsub("^[.]/", "", file.path(.dir, .all[.keep]))
}
