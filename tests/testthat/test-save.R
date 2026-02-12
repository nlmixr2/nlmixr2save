if (requireNamespace("withr", quietly = TRUE)) {
  if (requireNamespace("rxode2", quietly = TRUE)) {
    withr::with_options(list(nlmixr2.rxode2 = TRUE), {
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
    })

    withr::with_options(list(nlmixr2.rxode2 = FALSE), {
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
          expect_true(file.exists("testfit-rxUi.rds"))
          rm("ui")
          rxUi <- readRDS("testfit-rxUi.rds")
          expect_true(exists("rxUi"))
          expect_true(inherits(rxUi, "rxUi"))
        })
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
          if (is.raw(fitF$env[[n]])) {
            # the saved fit is never compressed interally
            expect_equal(eval(str2lang(paste0("fitF$", n))),
                         fit2F[[n]])
          } else {
            expect_equal(fitF$env[[n]],
                         fit2F$env[[n]])
          }

        })
      }
    }

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
      fit2S <- suppressMessages(loadFit("fitS"))

      fitEquals(fitF, fit2F)
      fitEquals(fitS, fit2S)

      fit2IF <- loadFit("fitIF")
      fitEquals(fitIF, fit2IF)

      fit2IS <- loadFit("fitIS")
      fitEquals(fitIS, fit2IS)

      ## test_that("saving and laoding a fit from nlmixr2 works", {

      ##   expect_true(file.exists("fitF.zip"))

      ##   fit2F <- loadFit("fitF")

      ##   expect_true(inherits(fit2F, "nlmixr2FitCore"))
      ##   expect_equal(coef(fitF), coef(fit2F))

      ##   expect_equal(fitF$parFixedDf, fit2F$parFixedDf)

      ##   expect_equal(fitF$ranef, fit2F$ranef)



      ##   expect_equal(fitF, fit2F)

      ##   suppressMessages(saveFit(fitS, "fitS"))
      ##   expect_true(file.exists("fitS.zip"))

      ##   fit2S <- loadFit("fitS")

      ##   expect_true(inherits(fit2S, "nlmixr2FitCore"))
      ##   expect_equal(coef(fitS), coef(fit2S))
      ##   expect_equal(fitS$parFixedDf, fit2S$parFixedDf)
      ##   expect_equal(fitS$ranef, fit2S$ranef)

      ## expect_equal(fitS, fit2S)


      ## })

    })
  }
}
