.skipIfNoS7Bind <- function() {
  skip_if_not_installed("S7", "1.0.0")
  skip_if_not(":=" %in% getNamespaceExports("S7"), "S7 has no `:=`")
}

test_that("the S7 attach hook is registered once (#18)", {
  .has <- vapply(
    getHook(packageEvent("S7", "attach")),
    function(f) isTRUE(attr(f, "nlmixr2save")),
    logical(1)
  )
  expect_equal(sum(.has), 1L)
})

test_that("`:=` works after S7 is attached after nlmixr2save (#18)", {
  skip_on_cran()
  .skipIfNoS7Bind()
  skip_if_not_installed("withr")
  .skipIfNotInstalledBuild()
  .script <- tempfile(fileext = ".R")
  on.exit(unlink(.script), add = TRUE)
  writeLines(
    c(
      "suppressPackageStartupMessages(library(nlmixr2save))",
      "s0 <- search()",
      "suppressPackageStartupMessages(library(S7))",
      "s <- search()",
      "stopifnot(identical(s, c('.GlobalEnv', 'package:nlmixr2save',",
      "  'package:S7', setdiff(s0, c('.GlobalEnv', 'package:nlmixr2save')))))",
      "stopifnot(environmentName(environment(`:=`)) == 'nlmixr2save')",
      # nlmixr2save's cache and S7's bind both work
      "withr::with_tempdir({",
      "  x := 1 + 1",
      "  stopifnot(identical(x, 2), file.exists('x.rds'))",
      "  Foo := new_class(properties=list(a=class_double))",
      "  describe := new_generic('x')",
      "  stopifnot(identical(Foo@name, 'Foo'), identical(describe@name, 'describe'))",
      "  stopifnot(identical(sort(list.files()), 'x.rds'))",
      "})",
      "cat('ok')"
    ),
    .script
  )
  .out <- .runScript(.script)
  expect_equal(tail(.out, 1), "ok", info = paste(.out, collapse = "\n"))
})

test_that("S7 attached before nlmixr2save is left where it is (#18)", {
  skip_on_cran()
  .skipIfNoS7Bind()
  skip_if_not_installed("withr")
  .skipIfNotInstalledBuild()
  .script <- tempfile(fileext = ".R")
  on.exit(unlink(.script), add = TRUE)
  writeLines(
    c(
      "suppressPackageStartupMessages({library(S7); library(nlmixr2save)})",
      "s <- search()",
      "stopifnot(identical(s[2:3], c('package:nlmixr2save', 'package:S7')))",
      "withr::with_tempdir({",
      "  Foo := new_class()",
      "  stopifnot(identical(Foo@name, 'Foo'), length(list.files()) == 0L)",
      "})",
      "cat('ok')"
    ),
    .script
  )
  .out <- .runScript(.script)
  expect_equal(tail(.out, 1), "ok", info = paste(.out, collapse = "\n"))
})

test_that("data.table without a `:=` after S7 does not move nlmixr2save (#18)", {
  skip_on_cran()
  .skipIfNoS7Bind()
  skip_if_not_installed("data.table")
  skip_if_not_installed("withr")
  .skipIfNotInstalledBuild()
  .script <- tempfile(fileext = ".R")
  on.exit(unlink(.script), add = TRUE)
  writeLines(
    c(
      # S7's conflictRules() keep data.table's `:=` off the search path, so
      # data.table masks nothing and the search path is left as library() made it
      "suppressPackageStartupMessages({library(S7); library(nlmixr2save)})",
      "s0 <- search()",
      "suppressPackageStartupMessages(library(data.table))",
      "stopifnot(!exists(':=', as.environment('package:data.table'),",
      "  inherits=FALSE))",
      "stopifnot(identical(search(), append(s0, 'package:data.table', 1)))",
      "stopifnot(environmentName(environment(`:=`)) == 'nlmixr2save')",
      "withr::with_tempdir({x := 1 + 1; stopifnot(identical(x, 2))})",
      "DT <- data.table(a=1:3)",
      "DT[, b := a * 2]",
      "stopifnot(identical(DT$b, c(2, 4, 6)))",
      "cat('ok')"
    ),
    .script
  )
  .out <- .runScript(.script)
  expect_equal(tail(.out, 1), "ok", info = paste(.out, collapse = "\n"))
})

test_that("`:=` hands S7::new_class() to S7 before S7 is loaded (#18)", {
  skip_on_cran()
  .skipIfNoS7Bind()
  skip_if_not_installed("withr")
  .skipIfNotInstalledBuild()
  .script <- tempfile(fileext = ".R")
  on.exit(unlink(.script), add = TRUE)
  writeLines(
    c(
      "suppressPackageStartupMessages(library(nlmixr2save))",
      "stopifnot(!isNamespaceLoaded('S7'))",
      "withr::with_tempdir({",
      "  Foo := S7::new_class()",
      "  stopifnot(identical(Foo@name, 'Foo'), length(list.files()) == 0L)",
      "})",
      "cat('ok')"
    ),
    .script
  )
  .out <- .runScript(.script)
  expect_equal(tail(.out, 1), "ok", info = paste(.out, collapse = "\n"))
})
