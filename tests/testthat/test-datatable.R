# The data.table hook is exercised in a fresh R session (data.table may
# already be attached in this one), which loads the *installed* nlmixr2save.
# Under devtools::test() that can be an older build than the source being
# tested, so only run those tests when the loaded namespace is the installed
# one (always true under R CMD check).
# Run a command in a child R process that sees the same library paths as this
# one (a runtime .libPaths() change is not inherited), so it loads the same
# nlmixr2save build.  R_LIBS is set in this process rather than through
# system2(env=), which only works on Unix.
.withLibs <- function(cmd, args) {
  withr::with_envvar(
    c(R_LIBS=paste(.libPaths(), collapse=.Platform$path.sep)),
    suppressWarnings(system2(cmd, args, stdout=TRUE, stderr=TRUE)))
}

# Run an R script in a fresh session
.runScript <- function(script) {
  .withLibs(file.path(R.home("bin"), "Rscript"),
            c("--vanilla", shQuote(script)))
}

.skipIfNotInstalledBuild <- function() {
  .installed <- tryCatch(find.package("nlmixr2save", lib.loc=.libPaths()),
                         error=function(e) "")
  .loaded <- getNamespaceInfo("nlmixr2save", "path")
  skip_if_not(identical(normalizePath(.installed, mustWork=FALSE),
                        normalizePath(.loaded, mustWork=FALSE)),
              "the loaded nlmixr2save is not the installed build")
}

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
  .skipIfNotInstalledBuild()
  # needs a fresh session, since data.table may already be attached here
  .script <- tempfile(fileext=".R")
  on.exit(unlink(.script), add=TRUE)
  writeLines(c(
    "suppressPackageStartupMessages(library(nlmixr2save))",
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
    # attaching data.table again with nlmixr2save already in front of it
    # leaves the order alone
    "detach('package:data.table')",
    "suppressPackageStartupMessages(library(data.table))",
    "stopifnot(identical(search(), s))",
    "stopifnot(environmentName(environment(`:=`)) == 'nlmixr2save')",
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
  .out <- .runScript(.script)
  expect_equal(tail(.out, 1), "ok", info=paste(.out, collapse="\n"))
})

test_that("nlmixr2save moves in front of data.table attached lower down", {
  skip_on_cran()
  skip_if_not_installed("data.table")
  skip_if_not_installed("withr")
  .skipIfNotInstalledBuild()
  .script <- tempfile(fileext=".R")
  on.exit(unlink(.script), add=TRUE)
  writeLines(c(
    "suppressPackageStartupMessages({library(nlmixr2save); library(tools)})",
    "s0 <- search()",
    "stopifnot(identical(s0[2:3], c('package:tools', 'package:nlmixr2save')))",
    # data.table lands below tools but above nlmixr2save
    "suppressPackageStartupMessages(library(data.table, pos=3))",
    "s <- search()",
    "stopifnot(identical(s[1:5], c('.GlobalEnv', 'package:tools',",
    "  'package:nlmixr2save', 'package:data.table', s0[4])))",
    "stopifnot(environmentName(environment(`:=`)) == 'nlmixr2save')",
    "cat('ok')"), .script)
  .out <- .runScript(.script)
  expect_equal(tail(.out, 1), "ok", info=paste(.out, collapse="\n"))
})

test_that("a package that Depends on nlmixr2save does not block the move", {
  skip_on_cran()
  skip_if_not_installed("data.table")
  skip_if_not_installed("withr")
  .skipIfNotInstalledBuild()
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
  .inst <- .withLibs(file.path(R.home("bin"), "R"),
                     c("CMD", "INSTALL", "-l", shQuote(.lib), shQuote(.pkg)))
  skip_if_not(dir.exists(file.path(.lib, "nlmixr2saveDep")),
              paste(c("could not install the test package:",
                      utils::tail(.inst, 5)), collapse="\n"))
  .script <- file.path(.dir, "run.R")
  writeLines(c(
    sprintf(".libPaths(c(%s, .libPaths()))", deparse(.lib)),
    "suppressPackageStartupMessages(library(nlmixr2saveDep))",
    "w <- NULL",
    "withCallingHandlers(suppressPackageStartupMessages(library(data.table)),",
    "  warning=function(e) {w <<- c(w, conditionMessage(e));",
    "    invokeRestart('muffleWarning')})",
    "stopifnot(is.null(w))",
    "s <- search()",
    "stopifnot(match('package:nlmixr2save', s) < match('package:data.table', s))",
    # nlmixr2save now comes before the package that Depends on it too;
    # dependents reach it through their namespace imports, not the search path
    "stopifnot(match('package:nlmixr2save', s) < match('package:nlmixr2saveDep', s))",
    "stopifnot(environmentName(environment(`:=`)) == 'nlmixr2save')",
    "stopifnot(depFun() == 1)",
    "cat('ok')"), .script)
  .out <- .runScript(.script)
  expect_equal(tail(.out, 1), "ok", info=paste(.out, collapse="\n"))
})

test_that("a strict conflicts.policy is left for library() to resolve", {
  skip_on_cran()
  skip_if_not_installed("data.table")
  skip_if_not_installed("withr")
  .skipIfNotInstalledBuild()
  .script <- tempfile(fileext=".R")
  on.exit(unlink(.script), add=TRUE)
  writeLines(c(
    "suppressPackageStartupMessages(library(nlmixr2save))",
    "options(conflicts.policy=list(error=TRUE))",
    # library() rolls back the failed attach; that must remove data.table,
    # not nlmixr2save
    "try(suppressPackageStartupMessages(library(data.table)), silent=TRUE)",
    "stopifnot('package:nlmixr2save' %in% search())",
    "stopifnot(!('package:data.table' %in% search()))",
    "for (p in c('strict', 'depends.ok')) {",
    "  options(conflicts.policy=p)",
    "  try(suppressPackageStartupMessages(library(data.table)), silent=TRUE)",
    "  stopifnot('package:nlmixr2save' %in% search())",
    "  stopifnot(!('package:data.table' %in% search()))",
    "}",
    "cat('ok')"), .script)
  .out <- .runScript(.script)
  expect_equal(tail(.out, 1), "ok", info=paste(.out, collapse="\n"))
})

test_that("a selective attach of nlmixr2save is re-attached as it was", {
  skip_on_cran()
  skip_if_not_installed("data.table")
  skip_if_not_installed("withr")
  .skipIfNotInstalledBuild()
  .script <- tempfile(fileext=".R")
  on.exit(unlink(.script), add=TRUE)
  writeLines(c(
    "suppressPackageStartupMessages(library(nlmixr2save,",
    "  include.only=c(':=', 'saveFit')))",
    "suppressPackageStartupMessages(library(data.table))",
    "s <- search()",
    "stopifnot(match('package:nlmixr2save', s) < match('package:data.table', s))",
    "stopifnot(setequal(ls('package:nlmixr2save'), c(':=', 'saveFit')))",
    "stopifnot(environmentName(environment(`:=`)) == 'nlmixr2save')",
    "detach('package:data.table'); detach('package:nlmixr2save')",
    # `:=` deliberately left out: data.table's stays, nothing moves
    "suppressPackageStartupMessages(library(nlmixr2save, exclude=':='))",
    "s0 <- search()",
    "suppressPackageStartupMessages(library(data.table))",
    "stopifnot(identical(search(), append(s0, 'package:data.table', 1)))",
    "stopifnot(!(':=' %in% ls('package:nlmixr2save')))",
    "stopifnot(environmentName(environment(`:=`)) == 'data.table')",
    "cat('ok')"), .script)
  .out <- .runScript(.script)
  expect_equal(tail(.out, 1), "ok", info=paste(.out, collapse="\n"))
})

test_that("a load_all() attach is left alone rather than lost", {
  skip_on_cran()
  skip_if_not_installed("data.table")
  skip_if_not_installed("withr")
  skip_if_not_installed("pkgload")
  # only from a source checkout (devtools::test()), not an installed package
  .src <- normalizePath(test_path("..", ".."), mustWork=FALSE)
  skip_if_not(file.exists(file.path(.src, "DESCRIPTION")) &&
                file.exists(file.path(.src, "R", "zzz.R")),
              "not running from the package source")
  .script <- tempfile(fileext=".R")
  on.exit(unlink(.script), add=TRUE)
  writeLines(c(
    sprintf("suppressMessages(pkgload::load_all(%s, quiet=TRUE))",
            deparse(.src)),
    "w <- NULL",
    "withCallingHandlers(suppressPackageStartupMessages(library(data.table)),",
    "  warning=function(e) {w <<- c(w, conditionMessage(e));",
    "    invokeRestart('muffleWarning')})",
    "stopifnot(is.null(w))",
    "stopifnot('package:nlmixr2save' %in% search())",
    "cat('ok')"), .script)
  .out <- .runScript(.script)
  expect_equal(tail(.out, 1), "ok", info=paste(.out, collapse="\n"))
})
