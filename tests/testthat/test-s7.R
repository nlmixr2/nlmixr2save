options(nlmixr2save.quiet = TRUE)

.skipIfNoS7Bind <- function() {
  skip_if_not_installed("S7", "1.0.0")
  skip_if_not(":=" %in% getNamespaceExports("S7"), "S7 has no `:=`")
}

test_that("`:=` binds S7 builders the way S7's `:=` does (#18)", {
  .skipIfNoS7Bind()
  skip_if_not_installed("withr")
  withr::with_tempdir({
    .env <- new.env()
    local(
      {
        Foo := S7::new_class(properties = list(x = S7::class_double))
        describe := S7::new_generic("x")
      },
      envir = .env
    )
    expect_true(inherits(.env$Foo, "S7_class"))
    expect_identical(.env$Foo@name, "Foo")
    expect_true(inherits(.env$describe, "S7_generic"))
    expect_identical(.env$describe@name, "describe")
    # bound, not cached
    expect_identical(list.files(all.files = TRUE, no.. = TRUE), character(0))
    expect_false(exists("Foo", envir = globalenv(), inherits = FALSE))
  })
})

test_that("`:=` resolves bare S7 builder names in the calling frame (#18)", {
  .skipIfNoS7Bind()
  skip_if_not_installed("withr")
  withr::with_tempdir({
    .f <- function() {
      new_class <- S7::new_class
      Bar := new_class()
      Bar
    }
    .bar <- .f()
    expect_identical(.bar@name, "Bar")
    expect_identical(list.files(all.files = TRUE, no.. = TRUE), character(0))
    # the value is returned invisibly, as with S7's `:=`
    expect_invisible(Baz := S7::new_class())
  })
})

test_that("S7's bind errors are kept (#18)", {
  .skipIfNoS7Bind()
  skip_if_not_installed("withr")
  withr::with_tempdir({
    expect_error(Foo := S7::new_class(name = "Other"), "name")
    expect_identical(list.files(all.files = TRUE, no.. = TRUE), character(0))
  })
})

test_that("`:=` still caches calls that are not S7 builders (#18)", {
  skip_if_not_installed("withr")
  withr::with_tempdir({
    # a user function with a `name` argument is not an S7 builder
    new_class <- function(name = "none") name
    x := new_class()
    expect_identical(x, "none")
    expect_true(file.exists("x.rds"))
    y := 1 + 1
    expect_identical(y, 2)
    expect_true(file.exists("y.rds"))
  })
})

test_that(".nlmixr2saveS7Bind() only matches S7 functions with `name`", {
  .skipIfNoS7Bind()
  .env <- globalenv()
  expect_null(.nlmixr2saveS7Bind(quote(x), .env))
  expect_null(.nlmixr2saveS7Bind(quote(1 + 1), .env))
  expect_null(.nlmixr2saveS7Bind(quote(stats::rnorm(1)), .env))
  expect_null(.nlmixr2saveS7Bind(quote(f()()), .env))
  # S7 functions that are not builders, or do not exist
  expect_null(.nlmixr2saveS7Bind(quote(S7::prop(x, "a")), .env))
  expect_null(.nlmixr2saveS7Bind(quote(S7::nothere()), .env))
  expect_identical(
    .nlmixr2saveS7Bind(quote(S7::new_class()), .env),
    getExportedValue("S7", ":=")
  )
  expect_identical(
    .nlmixr2saveS7Bind(str2lang("S7:::new_generic('x')"), .env),
    getExportedValue("S7", ":=")
  )
})
