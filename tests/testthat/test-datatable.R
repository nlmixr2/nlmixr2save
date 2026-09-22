test_that("nlmixr2save's `:=` is re-attached ahead of a masking one", {
  skip_if_not("package:nlmixr2save" %in% search())
  on.exit(.nlmixr2saveDetachAssign(), add=TRUE)
  expect_message(expect_true(.nlmixr2saveReattachAssign("data.table", "")),
                 "re-attached")
  expect_equal(search()[2], "nlmixr2save:assign")
  expect_identical(get(":=", envir=globalenv()),
                   get(":=", envir=asNamespace("nlmixr2save")))
  # calling it again replaces the entry rather than stacking a second one
  suppressMessages(.nlmixr2saveReattachAssign("data.table", ""))
  expect_equal(sum(search() == "nlmixr2save:assign"), 1L)
  expect_true(.nlmixr2saveDetachAssign())
  expect_false("nlmixr2save:assign" %in% search())
  expect_false(.nlmixr2saveDetachAssign())
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
    "if (!exists('.nlmixr2saveReattachAssign', asNamespace('nlmixr2save'))) {",
    "  cat('stale'); quit(save='no')",
    "}",
    "suppressPackageStartupMessages(library(data.table))",
    "stopifnot(environmentName(environment(`:=`)) == 'nlmixr2save')",
    "withr::with_tempdir({x := 1 + 1})",
    "stopifnot(identical(x, 2))",
    "DT <- data.table(a=1:3)",
    "DT[, b := a * 2]",
    "stopifnot(identical(DT$b, c(2, 4, 6)))",
    # detaching nlmixr2save hands `:=` back to data.table without dropping
    # the wrong search-path entry
    "detach('package:nlmixr2save')",
    "stopifnot(environmentName(environment(`:=`)) == 'data.table')",
    "stopifnot('package:data.table' %in% search())",
    # unloading removes the entry altogether
    "unloadNamespace('nlmixr2save')",
    "stopifnot(!('nlmixr2save:assign' %in% search()))",
    "detach('package:data.table')",
    # loaded but not attached: `:=` never reached nlmixr2save, so leave it
    "loadNamespace('nlmixr2save')",
    "suppressPackageStartupMessages(library(data.table))",
    "stopifnot(!('nlmixr2save:assign' %in% search()))",
    "stopifnot(environmentName(environment(`:=`)) == 'data.table')",
    "cat('ok')"), .script)
  .out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
                                   c("--vanilla", shQuote(.script)),
                                   stdout=TRUE, stderr=TRUE))
  skip_if(identical(tail(.out, 1), "stale"),
          "the installed nlmixr2save predates this fix")
  expect_equal(tail(.out, 1), "ok", info=paste(.out, collapse="\n"))
})
