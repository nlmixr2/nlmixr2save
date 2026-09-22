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
    "stopifnot(setequal(s, c(s0, 'package:data.table')))",
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
