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
          expect_true(exists("ui"))
          expect_true(inherits(ui, "rxUi"))
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
          ui <- readRDS("testfit-rxUi.rds")
          expect_true(exists("ui"))
          expect_true(inherits(ui, "rxUi"))
        })
      })
    })
  }


}

## test_that("saveFitItem handles data.frame rowDF and DF correctly", {
##   tmp <- tempdir()
##   oldwd <- getwd()
##   setwd(tmp)
##   on.exit(setwd(oldwd), add = TRUE)

##   df1 <- data.frame(a = 1:3)
##   res1 <- saveFitItem(df1, "parFixedDf", "testfit")
##   expect_true(res1)
##   expect_true(file.exists("testfit-parFixedDf.csv"))
##   df2 <- data.frame(b = 4:6)
##   res2 <- saveFitItem(df2, "ranef", "testfit")
##   expect_true(res2)
##   expect_true(file.exists("testfit-ranef.csv"))
##   # non-data.frame falling back to rds
##   obj <- list(x = 1)
##   res3 <- saveFitItem(obj, "fooObj", "testfit")
##   expect_true(res3)
##   expect_true(file.exists("testfit-fooObj.rds"))
## })
##
## test_that("saveFitItem.default returns FALSE for unknown types", {
##   tmp <- tempdir()
##   oldwd <- getwd()
##   setwd(tmp)
##   on.exit(setwd(oldwd), add = TRUE)

##   res <- saveFitItem(42L, "someInt", "testfit")
##   expect_false(res)
## })

## test_that("saveFit.nlmixr2FitCore writes expected files and loadFit reconstructs", {
##   tmp <- tempdir()
##   oldwd <- getwd()
##   setwd(tmp)
##   on.exit(setwd(oldwd), add = TRUE)

##   # prepare a fake fit object
##   fit <- list(env = new.env())
##   class(fit) <- "nlmixr2FitCore"
##   assign("parFixedDf", data.frame(a = 1:2), envir = fit$env)
##   assign("ranef", data.frame(b = 3:4), envir = fit$env)
##   assign("foo", list(hello = "world"), envir = fit$env)

##   # create a base csv that the generated loader expects
##   write.csv(data.frame(x = 10), "testfit.csv", row.names = FALSE)

##   saveFit.nlmixr2FitCore(fit, "testfit", zip = FALSE)

##   expect_true(file.exists("testfit-parFixedDf.csv"))
##   expect_true(file.exists("testfit-ranef.csv"))
##   expect_true(file.exists("testfit-foo.rds"))
##   expect_true(file.exists("testfit-env.R"))
##   expect_true(file.exists("testfit.R"))

##   loaded <- loadFit("testfit")
##   expect_true(is.data.frame(loaded))
##   expect_equal(loaded, read.csv("testfit.csv"))
##   expect_true(inherits(attr(loaded, ".foceiEnv"), "environment"))
## })
