test_that("saveFit errors on number", {
  expect_error(saveFit(1), "saveFit not implemented")
})

test_that(".assignParent errors on non-environment", {
  expect_error(.assignParent(1), "env must be an environment")
})

if (requireNamespace("withr", quietly = TRUE)) {

  test_that(":= with rxSolve requires seed to be set to restore", {
    withr::with_tempdir({

      rxode2::rxWithSeed(42, {

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

        rxode2::rxSetSeed(43)
        set.seed(43)
        solve42 := rxSolve(one.cmt, theo_sd)
        expect_false(.assignRestore())
        expect_false(identical(.new, rxode2::.rxGetSeed()))

        rxode2::rxSetSeed(43)
        set.seed(43)
        solve42 := rxSolve(one.cmt, theo_sd)
        expect_true(.assignRestore())

        rxode2::rxSetSeed(43)
        set.seed(43)
        solve42 := rxSolve(one.cmt, theo_sd, nStud=2)
        expect_false(.assignRestore())
        expect_false(identical(.new, rxode2::.rxGetSeed()))

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
      })
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

if (requireNamespace("nlmixr2est", quietly = TRUE) &&
      requireNamespace("nlmixr2data", quietly = TRUE)) {
  fitEquals <- function(fitF, fit2F) {
    fitName <- as.character(substitute(fitF))
    for (n in ls(fitF$env, all.names=TRUE)) {
      if (n == "ui") {
        for (m in names(fitF$ui)) {
          if (m == "control") {
            next
          }
          if (m %in% c("mv0", "mvL")) {
            test_that(paste0(fitName, "$env$ui$", m), {
              expect_equal(rxode2::rxNorm(fitF$ui[[m]]),
                           rxode2::rxNorm(fit2F$ui[[m]]))
            })
          } else {
            test_that(paste0(fitName, "$env$ui$", m), {
              expect_equal(fitF$ui[[m]], fit2F$ui[[m]])
            })
          }
        }
        next
      }
      if (n %in% c("foceiModel", "saemModel", "saem0")) {
        next
      }
      if (n == "omega") {
        test_that(paste0(fitName, " env item ", n, " matches after load"), {
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

          expect_equal(.omega, .omega2)
        })
        next
      }
      if (n %in% c("phiH", "phiC")) {
        test_that(paste0(fitName, " env item ", n, " matches after load"), {
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
          expect_equal(.phiHF, .phiH2F)
        })
        next
      }
      if (any(grepl("Control$", class(fitF$env[[n]])))) {
        f1 <- rxode2::rxUiDeparse(fitF$env[[n]], "ctl")
        f2 <- rxode2::rxUiDeparse(fit2F$env[[n]], "ctl")
        test_that(paste0("fitF env Control item", n, " match after load (using `rxUiDeparse()`)"), {
          expect_equal(f1, f2)
        })
        next
      }
      test_that(paste0(fitName, " env item ", n, " matches after load"), {
        if (is.raw(fitF$env[[n]]) ||
              is.raw(fit2F$env[[n]])) {
          # the saved fit is never compressed internally
          .fit1 <- eval(str2lang(paste0("fitF$", n)))
          .fit2 <- eval(str2lang(paste0("fit2F$", n)))
          expect_equal(.fit1, .fit2)
        } else {
          expect_equal(fitF$env[[n]],
                       fit2F$env[[n]])
        }

      })
    }
  }
  if (identical(Sys.getenv("NOT_CRAN"), "true")) {
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

      fit2IF <- loadFit("fitIF")
      fitEquals(fitIF, fit2IF)

      fit2IS <- loadFit("fitIS")
      fitEquals(fitIS, fit2IS)

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

  }
}
