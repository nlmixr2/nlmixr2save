skip_if_not_installed("nlmixr2est")
skip_if_not_installed("nlmixr2data")
skip_if_not(exists("rxEventEmit", envir = asNamespace("rxode2"), inherits = FALSE),
            "rxode2 has no event bus")

.rec <- new.env()
.listen <- function(env = parent.frame()) {
  .rec$ev <- list()
  rxode2::rxEventListen("nlmixr2save-test", function(event, ...) {
    .rec$ev[[length(.rec$ev) + 1L]] <- list(event = event, p = list(...))
  })
  withr::defer(rxode2::rxEventUnlisten("nlmixr2save-test"), envir = env)
}
.events <- function() vapply(.rec$ev, function(e) e$event, character(1))
.last <- function(event) {
  .i <- rev(which(.events() == event))[1]
  .rec$ev[[.i]]$p
}
.one <- function() {
  ini({
    tka <- log(1.57); tcl <- log(2.72); tv <- log(31.5)
    eta.ka ~ 0.6; eta.cl ~ 0.3; eta.v ~ 0.1; add.sd <- 0.7
  })
  model({
    ka <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl); v <- exp(tv + eta.v)
    linCmt() ~ add(add.sd)
  })
}

test_that("fit := nlmixr2() emits assignStart, fitComplete, assign; a rerun only assigns", {
  withr::local_package("nlmixr2est")
  withr::local_dir(withr::local_tempdir())
  .listen()
  fit := nlmixr2(.one, nlmixr2data::theo_sd, est = "posthoc")
  expect_identical(.events(), c("assignStart", "fitComplete", "assign"))
  expect_identical(.rec$ev[[1]]$p$name, "fit")
  .a <- .last("assign")
  expect_identical(.a$name, "fit")
  expect_false(.a$cached)
  expect_true(is.character(.a$sha))
  expect_true(inherits(.a$value, "nlmixr2FitCore"))
  .rec$ev <- list()
  fit := nlmixr2(.one, nlmixr2data::theo_sd, est = "posthoc")
  expect_identical(.events(), c("assignStart", "assign"))
  expect_true(.last("assign")$cached)
})

test_that("sim := rxSolve(fit) reports random, seed and since", {
  withr::local_package("nlmixr2est")
  withr::local_dir(withr::local_tempdir())
  fit <- suppressMessages(nlmixr2(.one, nlmixr2data::theo_sd, est = "posthoc"))
  .listen()
  ev <- rxode2::et(amt = 320) |> rxode2::et(0:24)
  set.seed(1)
  sim := rxode2::rxSolve(fit, ev, nSub = 2)
  expect_identical(.events(), c("assignStart", "solveComplete", "assign"))
  .a <- .last("assign")
  expect_true(.a$random)
  expect_false(is.null(.a$seed))
  expect_true(is.numeric(.a$since))
})

test_that("sha evaluation is silent: a call-valued argument logs once", {
  withr::local_package("nlmixr2est")
  withr::local_dir(withr::local_tempdir())
  .listen()
  a := augPred(nlmixr2(.one, nlmixr2data::theo_sd, est = "posthoc"))
  expect_equal(sum(.events() == "fitComplete"), 1L)
  expect_equal(sum(.events() == "assign"), 1L)
})

test_that("an erroring RHS emits assignAbort and restores the inAssign flag", {
  withr::local_package("nlmixr2est")
  withr::local_dir(withr::local_tempdir())
  .listen()
  expect_error(bad := nlmixr2(.one, nlmixr2data::theo_sd, est = "notAnEst"))
  expect_identical(.events(), c("assignStart", "assignAbort"))
  fit <- suppressMessages(nlmixr2(.one, nlmixr2data::theo_sd, est = "posthoc"))
  .rec$ev <- list()
  suppressWarnings(suppressMessages(saveFit(fit, "direct")))
  expect_identical(.events(), "assign")
  expect_identical(.last("assign")$name, "direct")
})

test_that("a := of a plain value and of a fit both emit assign", {
  withr::local_package("nlmixr2est")
  withr::local_dir(withr::local_tempdir())
  fit <- suppressMessages(nlmixr2(.one, nlmixr2data::theo_sd, est = "posthoc"))
  .listen()
  x := data.frame(a = 1)
  f2 := fit
  expect_identical(.events(), c("assignStart", "assign", "assignStart", "assign"))
  expect_identical(.last("assign")$name, "f2")
})

test_that("nested := keeps the outer sha", {
  withr::local_package("nlmixr2est")
  withr::local_dir(withr::local_tempdir())
  .listen()
  inner <- function() {
    y := data.frame(b = 2)
    data.frame(c = 3)
  }
  outer := inner()
  .as <- .rec$ev[.events() == "assign"]
  expect_identical(vapply(.as, function(e) e$p$name, ""), c("y", "outer"))
  expect_true(is.character(.as[[2]]$p$sha))
})
