options("nlmixr2save.quiet" = TRUE)

test_that("nlmixr2save.prefix/.dir place := cache files; .check=FALSE loads by existence", {
  skip_if_not_installed("nlmixr2est")
  skip_if_not_installed("nlmixr2data")
  withr::with_tempdir({
    library(nlmixr2est)
    library(nlmixr2data)
    withr::local_options(list(nlmixr2save.dir = "cache",
                              nlmixr2save.prefix = "vig-",
                              nlmixr2save.check = FALSE))

    one.cmt <- function() {
      ini({
        tka <- 0.45; tcl <- log(c(0, 2.7, 100)); tv <- 3.45
        eta.ka ~ 0.6; eta.cl ~ 0.3; eta.v ~ 0.1
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl); v <- exp(tv + eta.v)
        linCmt() ~ add(add.sd)
      })
    }

    ## first assignment: a cache miss -> fit + save a .zip under dir/prefix
    fit := suppressMessages(nlmixr(one.cmt, theo_sd, est = "focei",
                                   control = list(print = 0, compress = FALSE)))
    expect_false(.assignRestore())                       # was computed
    expect_true(file.exists(file.path("cache", "vig-fit.zip")))
    expect_s3_class(fit, "nlmixr2FitData")
    .obj1 <- fit$objDf$OBJF

    ## the prefix is only on the OUTER file: the zip internals use the bare fit
    ## name, so the archive stays a normal, interchangeable fit zip
    .inZip <- zip::zip_list(file.path("cache", "vig-fit.zip"))$filename
    expect_true(any(.inZip == "fit.R"))
    expect_false(any(grepl("vig-fit", .inZip)))

    ## second assignment: file exists -> loaded, not refit (check = FALSE)
    fit := stop("must not refit -- should load from cache")
    expect_true(.assignRestore())                        # was restored
    expect_equal(fit$objDf$OBJF, .obj1)

    ## a non-fit value caches as .rds (by result type)
    val := as.numeric(2 + 2)
    expect_false(.assignRestore())
    expect_true(file.exists(file.path("cache", "vig-val.rds")))
    val := stop("must not recompute")
    expect_true(.assignRestore())
    expect_equal(val, 4)

    ## nlmixr2saveInvalidate() clears everything under the active prefix/dir
    .removed <- nlmixr2saveInvalidate()
    expect_true(length(.removed) > 0)
    expect_false(file.exists(file.path("cache", "vig-fit.zip")))
    expect_false(file.exists(file.path("cache", "vig-val.rds")))

    ## and it only touches the active prefix
    writeLines("x", file.path("cache", "other-keep.rds"))
    nlmixr2saveInvalidate()
    expect_true(file.exists(file.path("cache", "other-keep.rds")))
  })
})
