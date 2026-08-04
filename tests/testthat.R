# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

suppressWarnings(library(testthat))
suppressWarnings(library(nlmixr2save))

# CRAN work-arounds (same policy as rxode2/nlmixr2est): CRAN allows two cores,
# and an uncapped rxode2/OpenMP thread pool makes `checking tests` report a CPU
# time several times the elapsed time, which is a NOTE on the incoming pretest.
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  rxode2::setRxThreads(1L)
  Sys.setenv(OMP_NUM_THREADS = "1")
  Sys.setenv(MKL_NUM_THREADS = "1")
}

test_check("nlmixr2save")
