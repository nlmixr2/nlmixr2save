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
}
