skip_if_not_installed("nlmixr2est")
skip_if_not_installed("nlmixr2data")

.oneCmt <- function() {
  ini({
    tka <- log(1.57)
    tcl <- log(2.72)
    tv <- log(31.5)
    eta.ka ~ 0.6
    eta.cl ~ 0.3
    eta.v ~ 0.1
    add.sd <- 0.7
    prop.sd <- 0.01
  })
  model({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    linCmt() ~ add(add.sd) + prop(prop.sd) + combined1()
  })
}
.fit <- function(model = .oneCmt, data = nlmixr2data::theo_sd, est = "posthoc") {
  suppressMessages(suppressWarnings(nlmixr2est::nlmixr2(model, data, est = est)))
}
.k <- function(x) digest::digest(x)

test_that("fit keys survive saveFit()/loadFit() and never write the fit", {
  withr::local_package("nlmixr2est")
  fit <- .fit()
  .before <- digest::digest(as.list.environment(fit$env, all.names = TRUE, sorted = TRUE))
  .id <- .k(nlmixr2saveFitIdentity(fit))
  .key <- .k(nlmixr2saveFitKey(fit, sha = FALSE))
  expect_identical(digest::digest(as.list.environment(fit$env, all.names = TRUE, sorted = TRUE)), .before)
  withr::with_tempdir({
    suppressWarnings(suppressMessages(saveFit(fit, "kfit")))
    back <- suppressMessages(loadFit("kfit"))
  })
  expect_identical(.k(nlmixr2saveFitIdentity(back)), .id)
  expect_identical(.k(nlmixr2saveFitKey(back, sha = FALSE)), .key)
})

test_that("keys tell apart different data and different error models", {
  fit <- .fit()
  .d <- nlmixr2data::theo_sd
  other <- .fit(data = .d[.d$ID != 1, ])
  expect_false(identical(.k(nlmixr2saveFitIdentity(fit)), .k(nlmixr2saveFitIdentity(other))))
  .m2 <- .oneCmt
  body(.m2) <- eval(call("substitute", body(.oneCmt), list(combined1 = as.name("combined2"))))
  fit2 <- .fit(model = .m2)
  expect_false(identical(.k(nlmixr2saveFitIdentity(fit)), .k(nlmixr2saveFitIdentity(fit2))))
})

test_that("addCwres keeps the identity but changes the cache key", {
  fit <- .fit(est = "saem")
  .id <- .k(nlmixr2saveFitIdentity(fit))
  .key <- .k(nlmixr2saveFitKey(fit))
  cw <- suppressMessages(suppressWarnings(nlmixr2est::addCwres(fit, updateObject = FALSE)))
  expect_identical(.k(nlmixr2saveFitIdentity(cw)), .id)
  expect_false(identical(.k(nlmixr2saveFitKey(cw)), .key))
})

test_that(":= results built on a fit re-run after the fit changes", {
  withr::local_package("nlmixr2est")
  withr::with_tempdir({
    fit := nlmixr2(.oneCmt, nlmixr2data::theo_sd, est = "saem")
    a := augPred(fit)
    a := augPred(fit)
    expect_true(.assignRestore())
    fit <- suppressMessages(suppressWarnings(addCwres(fit)))
    a := augPred(fit)
    expect_false(.assignRestore())
  })
})

test_that("a fit on the right of := hashes by its key in a new session", {
  skip_if_not_installed("callr")
  skip_on_cran()
  .dir <- withr::local_tempdir()
  .run <- function(dir) {
    suppressMessages({
      library(nlmixr2est)
      library(nlmixr2save)
    })
    setwd(dir)
    one <- function() {
      ini({
        tka <- log(1.57); tcl <- log(2.72); tv <- log(31.5)
        eta.ka ~ 0.6; eta.cl ~ 0.3; eta.v ~ 0.1; add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl); v <- exp(tv + eta.v)
        linCmt() ~ add(add.sd)
      })
    }
    fit := nlmixr2(one, nlmixr2data::theo_sd, est = "posthoc")
    a := augPred(fit)
    .assignRestore()
  }
  .lib <- .libPaths()
  expect_false(callr::r(.run, list(.dir), libpath = .lib))
  expect_true(callr::r(.run, list(.dir), libpath = .lib))
})

test_that("loadFit works without nlmixr2est attached", {
  skip_if_not_installed("callr")
  skip_on_cran()
  .dir <- withr::local_tempdir()
  fit <- .fit()
  withr::with_dir(.dir, suppressWarnings(suppressMessages(saveFit(fit, "noattach"))))
  .ok <- callr::r(function(dir) {
    setwd(dir)
    f <- suppressMessages(nlmixr2save::loadFit("noattach"))
    inherits(f, "nlmixr2FitCore")
  }, list(.dir), libpath = .libPaths())
  expect_true(.ok)
})
