
oldOpt <- getOption("nlmixr2save.quiet", FALSE)


options("nlmixr2save.quiet" = TRUE)

test_that("saveFit errors on number", {
  expect_error(saveFit(1), "saveFit not implemented")
})

test_that(".assignParent errors on non-environment", {
  expect_error(.assignParent(1), "env must be an environment")
})

test_that(".nlmixr2saveFitFiles matches one fit's files and no others", {
  # what comes back is zipped and then unlinked, so matching one file too many
  # destroys another cache and one too few leaves an unloadable one
  withr::with_tempdir({
    file.create(c("fit-env.R", "fit-ui.R", "fit.csv", "fit.R", "fit.zip",
                  "myfit-env.R", "myfit.csv", "fitExtra-env.R", "fit2.csv",
                  "my.fit-env.R", "my.fit.csv", "my_fit-env.R", "my_fit.csv",
                  "fit+1-env.R", ".dot-env.R", ".dot.csv"))

    expect_equal(sort(.nlmixr2saveFitFiles("fit")),
                 c("fit-env.R", "fit-ui.R", "fit.R", "fit.csv"))
    # not the .zip, and not a longer name that merely starts the same way
    expect_false(any(c("fit.zip", "fit2.csv", "fitExtra-env.R", "myfit.csv") %in%
                       .nlmixr2saveFitFiles("fit")))
    # a base name is a variable name, so it can hold regexp metacharacters:
    # as a pattern, "my.fit" would also match my_fit's files
    expect_equal(sort(.nlmixr2saveFitFiles("my.fit")),
                 c("my.fit-env.R", "my.fit.csv"))
    expect_equal(sort(.nlmixr2saveFitFiles("my_fit")),
                 c("my_fit-env.R", "my_fit.csv"))
    expect_equal(.nlmixr2saveFitFiles("fit+1"), "fit+1-env.R")
    # `.dot` is an ordinary R name, and its files are hidden
    expect_equal(sort(.nlmixr2saveFitFiles(".dot")), c(".dot-env.R", ".dot.csv"))
    expect_equal(length(.nlmixr2saveFitFiles("nosuch")), 0)

    dir.create("sub")
    file.create(c("sub/a-env.R", "sub/a.csv", "sub/ab.csv"))
    expect_equal(sort(.nlmixr2saveFitFiles(file.path("sub", "a"))),
                 c("sub/a-env.R", "sub/a.csv"))
  })
})

test_that("saveFitRandom adds and removes registered random functions", {
  .old <- saveFitRandom()
  on.exit(saveFitRandom(.old), add = TRUE)

  saveFitRandom(c("fooRandom", "pkg::barRandom"))
  .random <- saveFitRandom()

  expect_true(all(c("fooRandom", "barRandom") %in% .random))
  expect_equal(sum(.random == "fooRandom"), 1)
  expect_equal(sum(.random == "barRandom"), 1)

  saveFitRandom("barRandom", remove = TRUE)
  .random <- saveFitRandom()

  expect_true("fooRandom" %in% .random)
  expect_false("barRandom" %in% .random)
})

test_that("tracked-package version metadata helpers", {
  .cur <- .nlmixr2saveMeta()
  expect_true(is.list(.cur))
  expect_true(all(c("nlmixr2est", "rxode2", "nlmixr2save") %in% names(.cur)))
  expect_true(all(c("version", "sha") %in% names(.cur$nlmixr2est)))
  expect_true(all(c("version", "sha") %in% names(.cur$rxode2)))
  # metadata must deparse/reparse (it is embedded as text in the loader script)
  expect_equal(eval(parse(text = paste(deparse(.cur), collapse = "\n"))), .cur)

  .same <- list(nlmixr2est = list(version = "1.2.3", sha = NA_character_),
                rxode2 = list(version = "5.0.0", sha = NA_character_))
  .estDiff <- list(nlmixr2est = list(version = "9.9.9", sha = NA_character_),
                   rxode2 = list(version = "5.0.0", sha = NA_character_))
  .rxDiff <- list(nlmixr2est = list(version = "1.2.3", sha = NA_character_),
                  rxode2 = list(version = "6.0.0", sha = NA_character_))
  expect_false(.nlmixr2saveMetaDiffers(.same, .same))
  # a difference in EITHER tracked package is flagged
  expect_true(.nlmixr2saveMetaDiffers(.same, .estDiff))
  expect_true(.nlmixr2saveMetaDiffers(.same, .rxDiff))
  expect_equal(.nlmixr2saveChanged(.same, .estDiff), "nlmixr2est")
  expect_equal(.nlmixr2saveChanged(.same, .rxDiff), "rxode2")
  # nothing to compare -> no complaint (older saves, or package absent)
  expect_false(.nlmixr2saveMetaDiffers(NULL, .same))
  expect_false(.nlmixr2saveMetaDiffers(
    list(nlmixr2est = list(version = NA_character_)), .estDiff))
  # same version, different remote sha still counts as different
  .s1 <- list(rxode2 = list(version = "5.0.0", sha = "aaaaaaaaaaaa"))
  .s2 <- list(rxode2 = list(version = "5.0.0", sha = "bbbbbbbbbbbb"))
  expect_true(.nlmixr2saveMetaDiffers(.s1, .s2))

  expect_equal(.nlmixr2savePkgLabel(.same, "nlmixr2est"), "1.2.3")
  expect_equal(.nlmixr2savePkgLabel(.s1, "rxode2"), "5.0.0 (aaaaaaaaaa)")
  expect_equal(.nlmixr2savePkgLabel(NULL, "rxode2"), "(unknown)")
  # a bare version string (older metadata shape) is tolerated
  expect_equal(.nlmixr2savePkgLabel(list(rxode2 = "5.0.0"), "rxode2"), "5.0.0")

  expect_match(.nlmixr2saveVersionMsg(.same, .estDiff),
               "nlmixr2est 1.2.3 \\(installed 9.9.9\\)")
  expect_match(.nlmixr2saveVersionMsg(.same, .rxDiff),
               "rxode2 5.0.0 \\(installed 6.0.0\\)")
})

test_that("version-mismatch warning/rerun decision on a stub fit", {
  .env <- new.env(parent = emptyenv())
  assign(".nlmixr2saveMeta",
         list(nlmixr2est = list(version = "0.0.0-old", sha = NA_character_),
              rxode2 = list(version = "0.0.0-old", sha = NA_character_),
              nlmixr2save = "1"),
         envir = .env)
  class(.env) <- c("nlmixr2FitCore", "environment")
  expect_equal(.nlmixr2saveGetMeta(.env)$nlmixr2est$version, "0.0.0-old")

  # a fit with no stored metadata -> nothing to compare, no warning
  .noMeta <- new.env(parent = emptyenv())
  class(.noMeta) <- c("nlmixr2FitCore", "environment")
  expect_null(.nlmixr2saveGetMeta(.noMeta))
  expect_warning(.nlmixr2saveWarnVersion(.noMeta), NA)

  # stub whose stored versions differ from the installed packages: the
  # non-interactive branch warns and does not request a rerun
  skip_if_not_installed("nlmixr2est")
  expect_warning(.nlmixr2saveWarnVersion(.env),
                 "run with nlmixr2est 0.0.0-old")
  if (!interactive()) {
    expect_false(suppressWarnings(.nlmixr2saveVersionRerun(.env)))
  }
})

test_that("nlmixr2save.checkVersion option gates the check", {
  skip_if_not_installed("withr")
  expect_true(.nlmixr2saveCheckVersion())
  withr::with_options(list(nlmixr2save.checkVersion = FALSE), {
    expect_false(.nlmixr2saveCheckVersion())
    # loadFit's checkVersion argument defaults to the option
    expect_false(eval(formals(loadFit)$checkVersion))
  })
  withr::with_options(list(nlmixr2save.checkVersion = TRUE), {
    expect_true(.nlmixr2saveCheckVersion())
  })
})

if (requireNamespace("withr", quietly = TRUE)) {

  test_that("saveFitRandom marks registered functions as random", {
    .old <- saveFitRandom()
    on.exit(saveFitRandom(.old), add = TRUE)

    randomFun <- function() 1
    saveFitRandom(randomFun)

    withr::with_tempdir({
      rxode2::rxSetSeed(42)
      res := randomFun()
      .r <- readRDS("res.rds")
      expect_named(.r, c("ret", "sha1", "random", "old", "seed"))
    })
  })

  test_that(":= with rxSolve requires seed to be set to restore", {
    skip_on_cran()
    withr::with_tempdir({

      suppressWarnings(rxode2::rxWithSeed(42, {

        library(rxode2)
        library(nlmixr2data)

        one.cmt <- function() {
          ini({
            ## You may label each parameter with a comment
            tka <- 0.45 # Log Ka
            tcl <- log(c(0, 2.7, 100)) # Log Cl
            ## This works with interactive models
            ## You may also label the preceding line with label("label text")
            tv <- 3.45; label("log V")
            ## the label("Label name") works with all models
            eta.ka ~ 0.6
            eta.cl ~ 0.3
            eta.v ~ 0.1
            add.sd <- 0.7
          })
          model({
            ka <- exp(tka + eta.ka)
            cl <- exp(tcl + eta.cl)
            v <- exp(tv + eta.v)
            linCmt() ~ add(add.sd)
          })
        }

        rxode2::rxSetSeed(42)
        set.seed(42)
        solve42 := rxSolve(one.cmt, theo_sd)
        expect_false(.assignRestore())
        .new <- rxode2::.rxGetSeed()

        skip_if(!file.exists("solve42.rds"))

        .r <- readRDS("solve42.rds")
        expect_true(.r$random)
        expect_equal(.new, .r$seed)

        rxode2::rxSetSeed(42)
        set.seed(42)
        solve42 := rxSolve(one.cmt, theo_sd)
        expect_true(.assignRestore())
        expect_equal(.new, rxode2::.rxGetSeed())

        if (requireNamespace("nlmixr2est", quietly = TRUE)) {

          library(nlmixr2est)

          rxode2::rxSetSeed(42)
          set.seed(42)
          solveEst := nlmixr2(one.cmt, theo_sd, est="rxSolve")
          .new <- rxode2::.rxGetSeed()

          expect_false(.assignRestore())

          skip_if(!file.exists("solveEst.rds"))
          .r <- readRDS("solveEst.rds")
          expect_true(.r$random)
          expect_equal(.new, .r$seed)

          rxode2::rxSetSeed(42)
          set.seed(42)
          solveEst := nlmixr2(one.cmt, theo_sd, est="rxSolve")
          expect_true(.assignRestore())
          expect_equal(.new, rxode2::.rxGetSeed())

          if (requireNamespace("babelmixr2", quietly = TRUE) &&
                requireNamespace("PopED", quietly = TRUE)) {

            library(babelmixr2)
            library(PopED)

            f <- function() {
              ini({
                tKA <- 0.25
                tCL <- 3.75
                tV <- 72.8
                Favail <- fix(0.9)
                eta.ka ~ 0.09
                eta.cl ~ 0.25 ^ 2
                eta.v ~ 0.09
                prop.sd <- sqrt(0.04)
                add.sd <- sqrt(0.0025)
              })
              model({
                ka <- tKA * exp(eta.ka)
                v <- tV * exp(eta.v)
                cl <- tCL * exp(eta.cl)
                d/dt(depot) <- -ka * depot
                d/dt(central) <- ka * depot - cl / v * central
                cp <- central / v
                f(depot) <- DOSE * Favail
                cp ~ add(add.sd) + prop(prop.sd)
              })
            }

            f <- f() # compile/check nlmixr2/rxode2 model

            e <- et(amt=1, ii=24, until=250) %>%
              et(time=c(1,2,8,240,245)) %>%
              as.data.frame() %>%
              dplyr::mutate(low=c(NA_real_, 0, 0, 0, 240, 240),
                            high=c(NA_real_, 10, 10, 10, 248, 248))

            # Create a PopED database for `nlmixr2`:
            poped := nlmixr(f, e, "poped",
                            popedControl(a=list(c(DOSE=20),
                                                c(DOSE=40)),
                                         maxa=c(DOSE=200),
                                         mina=c(DOSE=0)))
            expect_true(file.exists("poped.rds"))
            expect_false(.assignRestore())

            poped := nlmixr(f, e, "poped",
                            popedControl(a=list(c(DOSE=20),
                                                c(DOSE=40)),
                                         maxa=c(DOSE=200),
                                         mina=c(DOSE=0)))
            expect_true(.assignRestore())

          }
        }
      }))
    })
  })

  withr::with_tempdir({
    test_that("test rxUi item saving with rxode2", {

      library(rxode2)

      ui <- function() {
        ini({
          CL <- 1
          V <- 10
        })
        model({
          k = CL / V
          d/dt(centr) <- -k * centr
        })
      }

      saveFitItem(ui(), "rxUi", "testfit")
      expect_true(file.exists("testfit-rxUi.R"))
      rm("ui")
      source("testfit-rxUi.R", local=TRUE)
      expect_true(exists("rxUi"))
      expect_true(inherits(rxUi, "rxUi"))

    })
  })
}

withr::with_tempdir({
  test_that("saveFitItem handles data.frame as expected", {

    df1 <- data.frame(a = 1:3, row.names = c("sub1", "sub2", "sub3"))
    res1 <- saveFitItem(df1, "parFixedDf", "testfit")
    expect_true(res1)
    expect_true(file.exists("testfit-parFixedDf.csv"))
    df2 <- read.csv("testfit-parFixedDf.csv", row.names=1)
    expect_equal(df1, df2)

    df1 <- data.frame(b = 4:6, row.names = c("sub1", "sub2", "sub3"))
    res2 <- saveFitItem(df1, "ranef", "testfit")
    expect_true(res2)
    expect_true(file.exists("testfit-ranef.csv"))
    df2 <- read.csv("testfit-ranef.csv")
    expect_false(identical(df1, df2))
    row.names(df1) <- NULL
    expect_equal(df1, df2)

    # data.frame not in special lists falling back to rds
    obj <- data.frame(x = 1)
    res3 <- saveFitItem(obj, "fooObj", "testfit")
    expect_true(res3)
    expect_true(file.exists("testfit-fooObj.rds"))

    obj2 <- readRDS("testfit-fooObj.rds")
    expect_equal(obj, obj2)

  })
})

withr::with_tempdir({
  test_that("nlmixr2saveParFixedDf restores both parFixedDf structures", {

    # numeric parFixedDf as produced by nlmixr2est; "CI Lower"/"CI Upper"
    # and "BSV(SD)" are all NA so read.csv() turns them into logicals
    df <- data.frame(
      Estimate = c(0.5, 1.0, 3.4),
      SE = c(0.2, NA, 0.05),
      `%RSE` = c(40, NA, 1.5),
      `Back-transformed` = c(1.6, 2.7, 30),
      `CI Lower` = rep(NA_real_, 3),
      `CI Upper` = rep(NA_real_, 3),
      `BSV(SD)` = rep(NA_real_, 3),
      `Shrink(SD)%` = c(1.5, 4.2, NA),
      check.names = FALSE,
      row.names = c("tka", "tcl", "add.sd"))

    # old structure (nlmixr2est <= 6.0): named Estimate/SE columns
    # (built as a list since `$<-.data.frame` drops names on columns)
    dfNamed <- as.list(df)
    dfNamed$Estimate <- stats::setNames(dfNamed$Estimate, row.names(df))
    dfNamed$SE <- stats::setNames(dfNamed$SE, row.names(df))
    dfNamed <- structure(dfNamed, class="data.frame", row.names=row.names(df))

    utils::write.csv(df, "parFixedDf.csv", row.names=TRUE)
    dfCsv <- read.csv("parFixedDf.csv", check.names=FALSE, row.names=1)
    expect_true(is.logical(dfCsv$`CI Lower`))

    expect_equal(nlmixr2saveParFixedDf(dfCsv), dfNamed)
    expect_equal(nlmixr2saveParFixedDf(dfCsv, named=TRUE), dfNamed)
    # new structure (nlmixr2est $parFixed refactor): unnamed Estimate/SE
    expect_equal(nlmixr2saveParFixedDf(dfCsv, named=FALSE), df)

  })
})

if (requireNamespace("nlmixr2est", quietly = TRUE) &&
      requireNamespace("nlmixr2data", quietly = TRUE)) {

  # Consolidated helper: compares all ui and env items in two fits using two
  # test_that blocks (instead of one per item) to avoid exhausting R's node
  # protection stack when testthat processes hundreds of accumulated results.
  fitEquals <- function(fitF, fit2F) {
    fitName <- as.character(substitute(fitF))

    test_that(paste0(fitName, " ui items match after load"), {
      for (m in names(fitF$ui)) {
        if (m == "control") next
        if (m %in% c("mv0", "mvL")) {
          expect_equal(rxode2::rxNorm(fitF$ui[[m]]),
                       rxode2::rxNorm(fit2F$ui[[m]]),
                       label = paste0(fitName, "$env$ui$", m))
        } else if (length(fitF$ui[[m]]) == 0L && length(fit2F$ui[[m]]) == 0L) {
          # empty ui slots (e.g. .muGroupCovNames) round-trip as character(0)
          # vs NULL depending on the nlmixr2est version; both are length 0 and
          # therefore consistent between the two implementations
          expect_equal(length(fitF$ui[[m]]), length(fit2F$ui[[m]]),
                       label = paste0(fitName, "$env$ui$", m, " (both empty)"))
        } else {
          expect_equal(fitF$ui[[m]], fit2F$ui[[m]],
                       label = paste0(fitName, "$env$ui$", m))
        }
      }
    })

    test_that(paste0(fitName, " env items match after load"), {
      for (n in ls(fitF$env, all.names=TRUE)) {
        if (n == "ui") next
        if (n %in% c("foceiModel", "saemModel", "saem0")) next
        if (n == "omega") {
          .omega <- fitF$env[[n]]
          .dn <- dimnames(.omega)
          attr(.omega, ".match.hash") <- NULL
          attr(.dn, ".match.hash") <- NULL
          attr(.dn[[1]], ".match.hash") <- NULL
          attr(.dn[[2]], ".match.hash") <- NULL
          dimnames(.omega) <- .dn

          .omega2 <- fit2F$env[[n]]
          .dn <- dimnames(.omega2)
          attr(.omega2, ".match.hash") <- NULL
          attr(.dn, ".match.hash") <- NULL
          attr(.dn[[1]], ".match.hash") <- NULL
          attr(.dn[[2]], ".match.hash") <- NULL
          dimnames(.omega2) <- .dn

          expect_equal(.omega, .omega2,
                       label = paste0(fitName, " env item omega"))
          next
        }
        if (n %in% c("phiH", "phiC")) {
          .phiHF <- fitF$env[[n]]
          .n <- names(.phiHF)
          .phiHF <- lapply(seq_along(.phiHF), function(i) {
            if (is.matrix(.phiHF[[i]])) {
              .dn <- dimnames(.phiHF[[i]])
              attr(.phiHF[[i]], ".match.hash") <- NULL
              attr(.dn, ".match.hash") <- NULL
              attr(.dn[[1]], ".match.hash") <- NULL
              attr(.dn[[2]], ".match.hash") <- NULL
              dimnames(.phiHF[[i]]) <- .dn
            }
            .phiHF[[i]]
          })
          names(.phiHF) <- .n

          .phiH2F <- fit2F$env[[n]]
          .n <- names(.phiH2F)
          .phiH2F <- lapply(seq_along(.phiH2F), function(i) {
            if (is.matrix(.phiH2F[[i]])) {
              .dn <- dimnames(.phiH2F[[i]])
              attr(.phiH2F[[i]], ".match.hash") <- NULL
              attr(.dn, ".match.hash") <- NULL
              attr(.dn[[1]], ".match.hash") <- NULL
              attr(.dn[[2]], ".match.hash") <- NULL
              dimnames(.phiH2F[[i]]) <- .dn
            }
            .phiH2F[[i]]
          })
          names(.phiH2F) <- .n
          expect_equal(.phiHF, .phiH2F,
                       label = paste0(fitName, " env item ", n))
          next
        }
        if (any(grepl("Control$", class(fitF$env[[n]])))) {
          f1 <- rxode2::rxUiDeparse(fitF$env[[n]], "ctl")
          f2 <- rxode2::rxUiDeparse(fit2F$env[[n]], "ctl")
          expect_equal(f1, f2,
                       label = paste0(fitName, " env item Control ", n))
          next
        }
        if (n == "iniDf0") {
          # Row names of iniDf0 may differ in type (integer vs character) after
          # CSV round-trip with row.names=1; normalize both sides to character
          .i1 <- fitF$env[[n]]
          .i2 <- fit2F$env[[n]]
          row.names(.i1) <- as.character(row.names(.i1))
          row.names(.i2) <- as.character(row.names(.i2))
          expect_equal(.i1, .i2,
                       label = paste0(fitName, " env item iniDf0"))
        } else if (is.raw(fitF$env[[n]]) || is.raw(fit2F$env[[n]])) {
          # the saved fit is never compressed internally
          .fit1 <- eval(str2lang(paste0("fitF$", n)))
          .fit2 <- eval(str2lang(paste0("fit2F$", n)))
          expect_equal(.fit1, .fit2,
                       label = paste0(fitName, " env item ", n))
        } else {
          expect_equal(fitF$env[[n]], fit2F$env[[n]],
                       label = paste0(fitName, " env item ", n))
        }
      }
    })
  }
  if (identical(Sys.getenv("NOT_CRAN"), "true") &&
        !nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_"))) {
    withr::with_tempdir({

      library(nlmixr2est)
      library(nlmixr2data)

      one.cmt <- function() {
        ini({
          ## You may label each parameter with a comment
          tka <- 0.45 # Log Ka
          tcl <- log(c(0, 2.7, 100)) # Log Cl
          ## This works with interactive models
          ## You may also label the preceding line with label("label text")
          tv <- 3.45; label("log V")
          ## the label("Label name") works with all models
          eta.ka ~ 0.6
          eta.cl ~ 0.3
          eta.v ~ 0.1
          add.sd <- 0.7
        })
        model({
          ka <- exp(tka + eta.ka)
          cl <- exp(tcl + eta.cl)
          v <- exp(tv + eta.v)
          linCmt() ~ add(add.sd)
        })
      }

      fitF <- suppressMessages(nlmixr(one.cmt, theo_sd, est="focei",
                                      control=list(print=0, compress=FALSE)))

      fitS <- suppressMessages(nlmixr(one.cmt, theo_sd, est="saem",
                                      control=list(print=0, compress=FALSE)))

      # now try iov
      theo_iov <- nlmixr2data::theo_md
      theo_iov$occ <- 1
      theo_iov$occ[theo_iov$TIME >= 144] <- 2

      one.cmt.iov <- function() {
        ini({
          tka <- 0.45 # Log Ka
          tcl <- log(c(0, 2.7, 100)) # Log Cl
          tv <- 3.45; label("log V")
          eta.ka ~ 0.6
          eta.cl ~ 0.3
          eta.v ~ 0.1
          iov.cl ~ 0.1 | occ
          add.sd <- 0.7
        })
        model({
          ka <- exp(tka + eta.ka)
          cl <- exp(tcl + eta.cl + iov.cl)
          v <- exp(tv + eta.v)
          linCmt() ~ add(add.sd)
        })
      }

      fitIF <- suppressMessages(nlmixr(one.cmt.iov, theo_iov, est="focei",
                                       control=list(print=0)))

      fitIS <- suppressMessages(nlmixr(one.cmt.iov, theo_iov, est="saem",
                                       control=list(print=0)))

      test_that("saving fits do not generate errors", {
        expect_error(suppressMessages(saveFit(fitS)), NA)
        expect_true(file.exists("fitS.zip"))

        expect_error(suppressMessages(saveFit(fitF, "fitF")), NA)
        expect_true(file.exists("fitF.zip"))

        expect_error(suppressMessages(saveFit(fitIF)), NA)
        expect_true(file.exists("fitIF.zip"))

        expect_error(suppressMessages(saveFit(fitIS)), NA)
        expect_true(file.exists("fitIS.zip"))
      })

      fit2F <- suppressMessages(loadFit("fitF"))
      fit2S <- suppressMessages(loadFit(fitS))

      fitEquals(fitF, fit2F)
      fitEquals(fitS, fit2S)

      test_that("a restored fit keeps ID a factor", {
        # the fit table round-trips through a plain .csv, so ID comes back as an
        # integer unless it is put back.  Anything joining the fit table to
        # something derived from the fit then hits a type mismatch --
        # nlme::augPred() keeps `id` a factor, and ggPMX::pmx_nlmixr() dies in a
        # data.table join on it.
        expect_true(is.factor(fit2F$ID))
        expect_equal(levels(fit2F$ID), levels(fitF$ID))
        expect_equal(as.character(fit2F$ID), as.character(fitF$ID))
        expect_true(is.factor(fit2S$ID))
        expect_equal(levels(fit2S$ID), levels(fitS$ID))
      })

      # The two repair functions are exercised on hand-built objects rather
      # than on another saved-and-reloaded fit.  Each extra round trip rebuilds
      # an rxode2 model, which is nearly free against a warm cache but costs
      # minutes against a cold one -- three such tests took over 18 minutes on
      # CI where all five fits together took 81 seconds.  Synthetic inputs also
      # pin the behavior down harder, since the levels can be made to disagree
      # with the row order in a way theo_sd's IDs never do.

      test_that(".nlmixr2saveRestoreIdFactor takes its levels from ranef", {
        .env <- new.env(parent=emptyenv())
        # levels deliberately in the opposite order to the rows, so following
        # ranef and following the order of appearance give different answers
        assign("ranef", data.frame(ID=factor(c("b", "a"), levels=c("b", "a"))),
               envir=.env)
        .cls <- c("nlmixr2FitData", "nlmixr2FitCore", "data.frame")
        .fit <- data.frame(ID=c("a", "b", "a"), DV=1:3)
        attr(.cls, ".foceiEnv") <- .env
        class(.fit) <- .cls

        .r <- .nlmixr2saveRestoreIdFactor(.fit)
        expect_true(is.factor(.r$ID))
        expect_equal(levels(.r$ID), c("b", "a"))
        # the labels still line up with the rows; only the coding changed
        expect_equal(as.character(.r$ID), c("a", "b", "a"))
        # the class attribute carries the env `$` dispatches through, and
        # column assignment must not drop it
        expect_true(is.environment(attr(class(.r), ".foceiEnv")))

        # with no usable ranef it falls back to the order the IDs appear, not
        # a sort -- a character sort would put "10" before "2"
        .env2 <- new.env(parent=emptyenv())
        assign("ranef", data.frame(ID=c(2L, 10L)), envir=.env2)
        .cls2 <- c("nlmixr2FitData", "nlmixr2FitCore", "data.frame")
        .fit2 <- data.frame(ID=c(2L, 10L, 2L), DV=1:3)
        attr(.cls2, ".foceiEnv") <- .env2
        class(.fit2) <- .cls2
        expect_equal(levels(.nlmixr2saveRestoreIdFactor(.fit2)$ID), c("2", "10"))

        # an ID the ranef levels do not cover must not become NA
        .env3 <- new.env(parent=emptyenv())
        assign("ranef", data.frame(ID=factor("a", levels="a")), envir=.env3)
        .cls3 <- c("nlmixr2FitData", "nlmixr2FitCore", "data.frame")
        .fit3 <- data.frame(ID=c("a", "z"), DV=1:2)
        attr(.cls3, ".foceiEnv") <- .env3
        class(.fit3) <- .cls3
        .r3 <- .nlmixr2saveRestoreIdFactor(.fit3)
        expect_false(anyNA(.r3$ID))
        expect_equal(levels(.r3$ID), c("a", "z"))

        # nothing to do for an object that is not a fit table with an ID
        expect_identical(.nlmixr2saveRestoreIdFactor(1L), 1L)
        expect_identical(.nlmixr2saveRestoreIdFactor(data.frame(a=1)),
                         data.frame(a=1))
      })

      test_that(".nlmixr2saveRestoreParHistType repairs a dropped type", {
        withr::with_tempdir({
          .e <- new.env(parent=emptyenv())
          .ph <- data.frame(iter=1:3,
                            type=c("Unscaled", "Unscaled", "Future Gradient"))
          utils::write.csv(.ph, "b-parHistData.csv", row.names=FALSE)
          # what the cache's own script leaves behind: a level list that
          # predates the type, so it came back NA
          .ph$type <- factor(.ph$type, levels="Unscaled")
          expect_true(anyNA(.ph$type))
          .cls <- class(.ph)
          attr(.cls, "niter") <- 42L   # saem hangs this off the class
          class(.ph) <- .cls
          assign("parHistData", .ph, envir=.e)

          .nlmixr2saveRestoreParHistType(.e, "b")
          .out <- get("parHistData", envir=.e)
          expect_false(anyNA(.out$type))
          expect_equal(levels(.out$type), c("Unscaled", "Future Gradient"))
          expect_equal(as.character(.out$type[3]), "Future Gradient")
          # every attribute nlmixr2est hangs off the class survives
          expect_equal(attr(class(.out), "niter"), 42L)

          # a type column with no NA is left exactly as it was
          .e2 <- new.env(parent=emptyenv())
          .ok <- data.frame(iter=1L, type=factor("Unscaled"))
          assign("parHistData", .ok, envir=.e2)
          .nlmixr2saveRestoreParHistType(.e2, "b")
          expect_identical(get("parHistData", envir=.e2), .ok)

          # a missing csv, or one whose rows do not line up, is left alone
          .e3 <- new.env(parent=emptyenv())
          assign("parHistData", .ph, envir=.e3)
          .nlmixr2saveRestoreParHistType(.e3, "nosuch")
          expect_true(anyNA(get("parHistData", envir=.e3)$type))

          utils::write.csv(.ph[1, ], "short-parHistData.csv", row.names=FALSE)
          .e4 <- new.env(parent=emptyenv())
          assign("parHistData", .ph, envir=.e4)
          .nlmixr2saveRestoreParHistType(.e4, "short")
          expect_true(anyNA(get("parHistData", envir=.e4)$type))

          # an env with no parHistData at all is fine
          expect_error(.nlmixr2saveRestoreParHistType(new.env(), "b"), NA)
        })
      })

      test_that("a cache saved before the levels were recorded still loads", {
        # A cache written by an earlier nlmixr2save has no `..id.level..` and no
        # `..parHistType.level..`.  Simulate one by blanking both out of the
        # env script (`env` is a plain environment when those lines run, so
        # assigning NULL leaves exactly what a missing entry looks like to the
        # restore script), and by injecting a parHistData type that postdates
        # the loader's hardcoded fallback list.
        suppressMessages(saveFit(fitS, "fitOld", zip=FALSE))
        # zip=FALSE has to be honored for a fit table too, not just for a core
        expect_false(file.exists("fitOld.zip"))
        expect_true(file.exists("fitOld-env.R"))
        expect_true(file.exists("fitOld.csv"))
        cat("env$`..id.level..` <- NULL\n",
            "env$`..parHistType.level..` <- NULL\n",
            file="fitOld-env.R", append=TRUE, sep="")

        .ph <- utils::read.csv("fitOld-parHistData.csv", check.names=FALSE)
        .ph$type[1] <- "Future Gradient"
        utils::write.csv(.ph, "fitOld-parHistData.csv", row.names=FALSE)

        .old <- suppressMessages(loadFit("fitOld", checkVersion=FALSE))

        # ID is repaired from the order the IDs appear, not from a sort --
        # a character sort would put "10" before "2".
        expect_true(is.factor(.old$ID))
        expect_equal(levels(.old$ID), unique(as.character(fitS$ID)))
        expect_equal(as.character(.old$ID), as.character(fitS$ID))

        # the unrecognized type is appended to the fallback list rather than
        # dropped to NA.  Source the restore script directly rather than going
        # through loadFit(): loadFit() also repairs an NA type from the csv,
        # which would mask a broken script.
        .se <- new.env()
        source("fitOld.R", local=.se)
        .script <- get("fitOld", envir=.se)
        expect_true(is.factor(.script$parHistData$type))
        expect_false(anyNA(.script$parHistData$type))
        expect_true("Future Gradient" %in% levels(.script$parHistData$type))
        expect_equal(as.character(.script$parHistData$type[1]), "Future Gradient")

        # and the same holds through loadFit()
        expect_false(anyNA(.old$parHistData$type))
        expect_equal(as.character(.old$parHistData$type[1]), "Future Gradient")
      })

      test_that("saveFit(data=FALSE) omits the original data", {
        suppressMessages(saveFit(fitF, "fitFnd", data=FALSE))
        expect_true(file.exists("fitFnd.zip"))
        .nd <- suppressMessages(loadFit("fitFnd", checkVersion=FALSE))
        expect_null(.nd$origData)
        # still a full FitData with its prediction columns
        expect_true(inherits(.nd, "nlmixr2FitData"))
        expect_true("IPRED" %in% names(.nd))
        # the nlmixr2save.data option drives the same behavior
        withr::with_options(list(nlmixr2save.data = FALSE),
                            suppressMessages(saveFit(fitF, "fitFndOpt")))
        .ndo <- suppressMessages(loadFit("fitFndOpt", checkVersion=FALSE))
        expect_null(.ndo$origData)
      })

      test_that("nlmixr2saveShare writes shareable zips and leaves the fit alone", {
        .clsBefore <- class(fitF)
        .rowsBefore <- nrow(fitF$origData)

        # from a live object -> fitF-noData.zip
        .p1 <- suppressMessages(nlmixr2saveShare(fitF))
        expect_true(file.exists("fitF-noData.zip"))
        .s1 <- suppressMessages(loadFit("fitF-noData", checkVersion=FALSE))
        expect_null(.s1$origData)
        expect_true(inherits(.s1, "nlmixr2FitData"))

        # noFit=TRUE -> only fitF-noData-noFit.zip, loads as a core
        .p2 <- suppressMessages(nlmixr2saveShare(fitF, noFit = TRUE))
        expect_true(file.exists("fitF-noData-noFit.zip"))
        .s2 <- suppressMessages(loadFit("fitF-noData-noFit", checkVersion=FALSE))
        expect_true(inherits(.s2, "nlmixr2FitCore"))
        expect_false(inherits(.s2, "nlmixr2FitData"))
        expect_false(inherits(.s2, "data.frame"))
        expect_null(.s2$origData)
        # eta/parameter-history tables and estimates are kept
        expect_false(is.null(.s2$etaObf))
        expect_false(is.null(.s2$parHistData))
        expect_false(is.null(.s2$parFixed))

        # reading from an existing zip base name also works
        .p3 <- suppressMessages(nlmixr2saveShare("fitF"))
        expect_true(file.exists("fitF-noData.zip"))

        # the original fit object is unchanged (env is shared by reference)
        expect_identical(class(fitF), .clsBefore)
        expect_true(is.environment(attr(class(fitF), ".foceiEnv")))
        expect_identical(nrow(fitF$origData), .rowsBefore)
      })

      test_that("nlmixr2saveShare honors nlmixr2save.dir / prefix", {
        withr::with_options(list(nlmixr2save.dir = "shareCache",
                                 nlmixr2save.prefix = "sh-"), {
          .p <- suppressMessages(nlmixr2saveShare(fitF))
          expect_true(file.exists(file.path("shareCache", "sh-fitF-noData.zip")))
        })
      })

      fit2IF <- loadFit("fitIF")
      fitEquals(fitIF, fit2IF)

      fit2IS <- loadFit("fitIS")
      fitEquals(fitIS, fit2IS)

      test_that("a compressed fit still records its parHistData type levels", {
        # nlmixr2est stores parHistData compressed (a raw vector in the env)
        # unless compress=FALSE, so saveFit() has to decompress before it can
        # read the type levels off it.  Without that it fell through to the
        # loader's hardcoded level list, which nlmixr2est has since outgrown
        # ("Analytic Gradient (relaxed)" and friends), and those levels came
        # back as NA.
        expect_true(is.raw(get("parHistData", envir=fitIS$env)))
        expect_equal(levels(fit2IS$parHistData$type),
                     levels(fitIS$parHistData$type))
        expect_false(anyNA(fit2IS$parHistData$type))
        expect_equal(levels(fit2IF$parHistData$type),
                     levels(fitIF$parHistData$type))
        expect_false(anyNA(fit2IF$parHistData$type))
      })

      one.cmt.nlm <- function() {
        ini({
          tka <- 0.45 # Log Ka
          tcl <- log(c(0, 2.7, 100)) # Log Cl
          tv <- 3.45; label("log V")
          add.sd <- 0.7
        })
        model({
          ka <- exp(tka)
          cl <- exp(tcl)
          v <- exp(tv)
          linCmt() ~ add(add.sd)
        })
      }

      fitNL <- suppressMessages(nlmixr(one.cmt.nlm, theo_sd, est="nlm",
                                       control=list(print=0, compress=FALSE)))

      test_that("saving fits do not generate errors", {
        expect_error(suppressMessages(saveFit(fitNL, "fitNL")), NA)
        expect_true(file.exists("fitNL.zip"))
      })

      fit2NL <- loadFit("fitNL")
      fitEquals(fitNL, fit2NL)

      fitNL2 <- suppressMessages(nlmixr(one.cmt.nlm, theo_sd, est="nlm",
                                        control=list(print=0, compress=FALSE,
                                                     calcTables=FALSE)))

      test_that("saving fits do not generate errors", {
        expect_error(suppressMessages(saveFit(fitNL2)), NA)
        expect_true(file.exists("fitNL2.zip"))
      })

      fit2NL2 <- loadFit("fitNL2")
      fitEquals(fitNL2, fit2NL2)
    })


    test_that("test assignment", {

      suppressMessages(withr::with_tempdir({

        library(nlmixr2est)
        library(nlmixr2data)

        one.cmt <- function() {
          ini({
            ## You may label each parameter with a comment
            tka <- 0.45 # Log Ka
            tcl <- log(c(0, 2.7, 100)) # Log Cl
            ## This works with interactive models
            ## You may also label the preceding line with label("label text")
            tv <- 3.45; label("log V")
            ## the label("Label name") works with all models
            eta.ka ~ 0.6
            eta.cl ~ 0.3
            eta.v ~ 0.1
            add.sd <- 0.7
          })
          model({
            ka <- exp(tka + eta.ka)
            cl <- exp(tcl + eta.cl)
            v <- exp(tv + eta.v)
            linCmt() ~ add(add.sd)
          })
        }

        fitF := nlmixr(one.cmt, theo_sd, est="focei",
                       control=list(print=0, compress=FALSE))

        fitF2 := fitF
        expect_true(file.exists("fitF2.zip"))
        hash0 <- tools::md5sum("fitF.zip")

        expect_true(file.exists("fitF.zip"))
        hash1 <- tools::md5sum("fitF.zip")

        fitF := nlmixr(one.cmt, theo_sd, est="focei",
                       control=list(print=0, compress=FALSE))
        expect_true(file.exists("fitF.zip"))
        hash2 <- tools::md5sum("fitF.zip")

        expect_equal(hash1, hash2)

        fitF := nlmixr(one.cmt, theo_sd, est="saem",
                       control=list(print=0, compress=FALSE))

        expect_true(file.exists("fitF.zip"))
        hash3 <- tools::md5sum("fitF.zip")
        expect_false(identical(hash1, hash3))

        # Without tables
        fitF := nlmixr(one.cmt, theo_sd, est="focei",
                       control=list(print=0, compress=FALSE, calcTables=FALSE))

        expect_true(file.exists("fitF.zip"))
        hash4 <- tools::md5sum("fitF.zip")

        expect_false(identical(hash1, hash4))

        fitF := nlmixr(one.cmt, theo_sd, est="focei",
                       control=list(print=0, compress=FALSE, calcTables=FALSE))

        expect_true(file.exists("fitF.zip"))
        hash5 <- tools::md5sum("fitF.zip")

        expect_equal(hash5, hash4)
        fitF := nlmixr(one.cmt, theo_sd, est="saem",
                       control=list(print=0, compress=FALSE))

        expect_true(file.exists("fitF.zip"))
        hash6 <- tools::md5sum("fitF.zip")
        expect_false(identical(hash4, hash6))


      }))
    })

    test_that("dataset changes: irrelevant column does not refit; DV change does", {

      suppressMessages(withr::with_tempdir({

        library(nlmixr2est)
        library(nlmixr2data)

        one.cmt <- function() {
          ini({
            tka <- 0.45
            tcl <- log(c(0, 2.7, 100))
            tv <- 3.45
            eta.ka ~ 0.6
            eta.cl ~ 0.3
            eta.v ~ 0.1
            add.sd <- 0.7
          })
          model({
            ka <- exp(tka + eta.ka)
            cl <- exp(tcl + eta.cl)
            v  <- exp(tv + eta.v)
            linCmt() ~ add(add.sd)
          })
        }

        # Baseline fit
        fitD := nlmixr(one.cmt, theo_sd, est="focei",
                       control=list(print=0, compress=FALSE))
        expect_false(.assignRestore())
        hash_base <- tools::md5sum("fitD.zip")

        # Adding an irrelevant column should restore from cache unchanged
        theo_sd_extra <- theo_sd
        theo_sd_extra$.ignored <- "noise"
        fitD := nlmixr(one.cmt, theo_sd_extra, est="focei",
                       control=list(print=0, compress=FALSE))
        expect_true(.assignRestore())
        hash_extra <- tools::md5sum("fitD.zip")
        expect_equal(hash_base, hash_extra)
        # origData in the restored fit reflects the new (extra-column) data
        expect_equal(fitD$origData, theo_sd_extra)

        # Changing DV values must trigger a refit
        theo_sd_dv <- theo_sd
        theo_sd_dv$DV <- theo_sd_dv$DV + 1
        fitD := nlmixr(one.cmt, theo_sd_dv, est="focei",
                       control=list(print=0, compress=FALSE))
        expect_false(.assignRestore())
        hash_dv <- tools::md5sum("fitD.zip")
        expect_false(identical(hash_base, hash_dv))

      }))
    })

  }
}

options("nlmixr2save.quiet" = oldOpt)
