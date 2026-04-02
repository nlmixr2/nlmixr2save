pheno <- function() {
  ini({
    lcl <- log(0.008); label("Typical value of clearance")
    lvc <-  log(0.6); label("Typical value of volume of distribution")
    etalcl + etalvc ~ c(1,
                        0.01, 1)
    cpaddSd <- 0.1; label("residual variability")
  })
  model({
    cl <- exp(lcl + etalcl)*WT/70
    vc <- exp(lvc + etalvc)
    kel <- cl/vc
    d/dt(central) <- -kel*central
    cp <- central/vc
    cp ~ add(cpaddSd)
  })
}

modelSimple <- rxode2::rxode2(pheno)
test_that("nlmixrDataSimplify", {
  # Columns are kept in the correct order
  expect_equal(
    names(nlmixrDataSimplify(data = nlmixr2data::pheno_sd, object = modelSimple)),
    c("id", "time", "amt", "dv", "mdv", "evid", "WT"))
  # table's 'keep' argument is respected
  expect_equal(
    names(nlmixrDataSimplify(
      data = nlmixr2data::pheno_sd,
      object = modelSimple,
      table = nlmixr2est::tableControl(keep = "APGR")
    )),
    c("id", "time", "amt", "dv", "mdv", "evid", "APGR", "WT")
  )
  # duplication between table's 'keep' argument and covariates does not
  # duplicate columns
  expect_equal(
    names(nlmixrDataSimplify(
      data = nlmixr2data::pheno_sd,
      object = modelSimple,
      table = nlmixr2est::tableControl(keep = "WT")
    )),
    c("id", "time", "amt", "dv", "mdv", "evid", "WT")
  )
  # duplication between table's 'keep' argument and nlmixr2 columns does not add
  # them
  expect_equal(
    names(nlmixrDataSimplify(
      data = nlmixr2data::pheno_sd,
      object = modelSimple,
      table = nlmixr2est::tableControl(keep = "MDV")
    )),
    c("id", "time", "amt", "dv", "mdv", "evid", "WT")
  )
})

test_that("nlmixrDataSimplify expected errors", {
  badDataLowerCase <- nlmixr2data::pheno_sd
  badDataLowerCase$id <- badDataLowerCase$ID
  expect_error(
    nlmixrDataSimplify(data = badDataLowerCase, object = modelSimple),
    regexp = "the following column(s) are duplicated when lower case: 'id'",
    fixed = TRUE
  )
  badDataNoCov <- nlmixr2data::pheno_sd
  badDataNoCov$WT <- NULL
  expect_error(
    nlmixrDataSimplify(data = badDataNoCov, object = modelSimple),
    regexp = "the following covariate column(s) are missing from the data: 'WT'",
    fixed = TRUE
  )
})

test_that("re-estimating a model works with covariates (#9)", {
  badDataLowerCase <- nlmixr2data::pheno_sd
  badDataLowerCase$id <- badDataLowerCase$ID
  fitEstimated <-
    suppressMessages(
      nlmixr2est::nlmixr(
        object = modelSimple,
        data = nlmixr2data::pheno_sd,
        est = "focei",
        control = list(eval.max = 1)
      )
    )
  expect_true(
    "WT" %in% names(nlmixrDataSimplify(data = nlmixr2data::pheno_sd, object = fitEstimated))
  )
})
