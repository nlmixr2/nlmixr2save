test_that("nlmixr2save is only re-attached when data.table is ahead of it", {
  # already ahead, or data.table not attached: leave the search path alone
  .search <- search()
  if (!("package:data.table" %in% .search) ||
        match("package:nlmixr2save", .search, 0L) <
          match("package:data.table", .search)) {
    expect_false(.nlmixr2saveReattach("data.table", ""))
    expect_identical(search(), .search)
  }
})

test_that("the data.table attach hook is registered once", {
  .has <- vapply(getHook(packageEvent("data.table", "attach")),
                 function(f) isTRUE(attr(f, "nlmixr2save")), logical(1))
  expect_equal(sum(.has), 1L)
})

test_that("`:=` works after data.table is attached after nlmixr2save (#8)", {
  skip_on_cran()
  skip_if_not_installed("data.table")
  skip_if_not_installed("withr")
  # needs a fresh session, since data.table may already be attached here
  .script <- tempfile(fileext=".R")
  on.exit(unlink(.script), add=TRUE)
  writeLines(c(
    "suppressPackageStartupMessages(library(nlmixr2save))",
    "if (!exists('.nlmixr2saveReattach', asNamespace('nlmixr2save'))) {",
    "  cat('stale'); quit(save='no')",
    "}",
    "s0 <- search()",
    "suppressPackageStartupMessages(library(data.table))",
    "s <- search()",
    "stopifnot(match('package:nlmixr2save', s) < match('package:data.table', s))",
    "stopifnot(environmentName(environment(`:=`)) == 'nlmixr2save')",
    "withr::with_tempdir({x := 1 + 1})",
    "stopifnot(identical(x, 2))",
    "DT <- data.table(a=1:3)",
    "DT[, b := a * 2]",
    "stopifnot(identical(DT$b, c(2, 4, 6)))",
    # only nlmixr2save moves; nothing is dropped or duplicated
    "stopifnot(sum(s == 'package:nlmixr2save') == 1L)",
    "stopifnot(identical(s, c('.GlobalEnv', 'package:nlmixr2save',",
    "  'package:data.table', setdiff(s0, c('.GlobalEnv', 'package:nlmixr2save')))))",
    # a later detach of either package behaves normally
    "detach('package:nlmixr2save')",
    "stopifnot(environmentName(environment(`:=`)) == 'data.table')",
    "stopifnot('package:data.table' %in% search())",
    "detach('package:data.table')",
    # loaded but not attached: `:=` never reached nlmixr2save, so leave it
    "suppressPackageStartupMessages(library(data.table))",
    "stopifnot(!('package:nlmixr2save' %in% search()))",
    "stopifnot(environmentName(environment(`:=`)) == 'data.table')",
    "cat('ok')"), .script)
  .out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
                                   c("--vanilla", shQuote(.script)),
                                   stdout=TRUE, stderr=TRUE))
  skip_if(identical(tail(.out, 1), "stale"),
          "the installed nlmixr2save predates this fix")
  expect_equal(tail(.out, 1), "ok", info=paste(.out, collapse="\n"))
})

test_that("nlmixr2save moves in front of data.table attached lower down", {
  skip_on_cran()
  skip_if_not_installed("data.table")
  .script <- tempfile(fileext=".R")
  on.exit(unlink(.script), add=TRUE)
  writeLines(c(
    "suppressPackageStartupMessages({library(tools); library(nlmixr2save)})",
    "if (!exists('.nlmixr2saveReattach', asNamespace('nlmixr2save'))) {",
    "  cat('stale'); quit(save='no')",
    "}",
    "s0 <- search()",
    # data.table lands below tools but above nlmixr2save
    "suppressPackageStartupMessages(library(data.table, pos=4))",
    "s <- search()",
    "stopifnot(identical(s[1:5], c('.GlobalEnv', 'package:nlmixr2save',",
    "  'package:tools', 'package:data.table', s0[4])))",
    "stopifnot(environmentName(environment(`:=`)) == 'nlmixr2save')",
    "cat('ok')"), .script)
  .out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
                                   c("--vanilla", shQuote(.script)),
                                   stdout=TRUE, stderr=TRUE))
  skip_if(identical(tail(.out, 1), "stale"),
          "the installed nlmixr2save predates this fix")
  expect_equal(tail(.out, 1), "ok", info=paste(.out, collapse="\n"))
})

test_that("a package that Depends on nlmixr2save does not block the move", {
  skip_on_cran()
  skip_if_not_installed("data.table")
  # detach() refuses to detach a package another attached package Depends on
  .dir <- tempfile()
  on.exit(unlink(.dir, recursive=TRUE), add=TRUE)
  .lib <- file.path(.dir, "lib")
  .pkg <- file.path(.dir, "nlmixr2saveDep")
  dir.create(.lib, recursive=TRUE)
  dir.create(file.path(.pkg, "R"), recursive=TRUE)
  writeLines(c("Package: nlmixr2saveDep", "Version: 0.0.1",
               "Title: Test", "Description: Test.", "License: MIT",
               "Author: a", "Maintainer: a <a@example.com>",
               "Depends: nlmixr2save"),
             file.path(.pkg, "DESCRIPTION"))
  writeLines("depFun <- function() 1", file.path(.pkg, "R", "f.R"))
  writeLines("export(depFun)", file.path(.pkg, "NAMESPACE"))
  .inst <- suppressWarnings(system2(file.path(R.home("bin"), "R"),
                                    c("CMD", "INSTALL", "-l", shQuote(.lib),
                                      shQuote(.pkg)),
                                    stdout=TRUE, stderr=TRUE))
  skip_if_not(dir.exists(file.path(.lib, "nlmixr2saveDep")),
              "could not install the test package")
  .script <- file.path(.dir, "run.R")
  writeLines(c(
    sprintf(".libPaths(c(%s, .libPaths()))", deparse(.lib)),
    "suppressPackageStartupMessages(library(nlmixr2saveDep))",
    "if (!exists('.nlmixr2saveReattach', asNamespace('nlmixr2save'))) {",
    "  cat('stale'); quit(save='no')",
    "}",
    "w <- NULL",
    "withCallingHandlers(suppressPackageStartupMessages(library(data.table)),",
    "  warning=function(e) {w <<- c(w, conditionMessage(e));",
    "    invokeRestart('muffleWarning')})",
    "stopifnot(is.null(w))",
    "s <- search()",
    "stopifnot(match('package:nlmixr2save', s) < match('package:data.table', s))",
    "stopifnot('package:nlmixr2saveDep' %in% s)",
    "stopifnot(environmentName(environment(`:=`)) == 'nlmixr2save')",
    "stopifnot(depFun() == 1)",
    "cat('ok')"), .script)
  .out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
                                   c("--vanilla", shQuote(.script)),
                                   stdout=TRUE, stderr=TRUE))
  skip_if(identical(tail(.out, 1), "stale"),
          "the installed nlmixr2save predates this fix")
  expect_equal(tail(.out, 1), "ok", info=paste(.out, collapse="\n"))
})
