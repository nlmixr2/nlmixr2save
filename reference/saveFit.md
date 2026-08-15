# Save a fitted model object to a series of files

Save a fitted model object to a series of files

## Usage

``` r
saveFit(fit, file, zip = TRUE, data = .nlmixr2saveData())

# S3 method for class 'nlmixr2FitCore'
saveFit(fit, file, zip = TRUE, data = .nlmixr2saveData())

# S3 method for class 'nlmixr2FitData'
saveFit(fit, file, zip = TRUE, data = .nlmixr2saveData())

# Default S3 method
saveFit(fit, file, zip = TRUE, data = .nlmixr2saveData())
```

## Arguments

- fit:

  the fitted model object

- file:

  the base name of the files to save the fit to.

- zip:

  Boolean indicating if the files should be zipped.

- data:

  Boolean indicating whether the original dataset (`origData`) is stored
  in the saved fit. When `FALSE` it is omitted, producing a fit that can
  be shared without the subject-level data (see
  [`nlmixr2saveShare()`](nlmixr2saveShare.md)). Defaults to
  `getOption("nlmixr2save.data", TRUE)`.

## Value

nothing, called for side effects

## Author

Matthew L. Fidler

## Examples

``` r
# \donttest{
  if (requireNamespace("nlmixr2est", quietly=TRUE) &&
        requireNamespace("nlmixr2data", quietly=TRUE) &&
        requireNamespace("withr")) {
    library(nlmixr2est)
    library(nlmixr2data)
    withr::with_tempdir({
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

      fit <- nlmixr2(one.cmt, theo_sd, est="focei")

      saveFit(fit) # saved to fit.zip
      fit2 <- loadFit(fit) # load fit.zip

      if (file.exists("fit.zip")) {
         unlink("fit.zip")
      }

      print(fit2)
    })
  }
#>  
#>  
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> → Calculating residuals/tables
#> ✔ done
#> ℹ saving fit item: .fdFullCov
#> ℹ saving fit item: .fdFullS
#> ℹ saving fit item: .rownum
#> ℹ saving fit item: AIC
#> ℹ saving fit item: BIC
#> ℹ saving fit item: R
#> ℹ saving fit item: R.0
#> ℹ saving fit item: R.E
#> ℹ saving fit item: R.pd
#> ℹ saving fit item: Rinv
#> ℹ saving fit item: S
#> ℹ saving fit item: S.E
#> ℹ saving fit item: S.pd
#> ℹ saving fit item: S0
#> ℹ saving fit item: Sper
#> ℹ saving fit item: aqHi
#> ℹ saving fit item: aqLow
#> ℹ saving fit item: aqn
#> ℹ saving fit item: censInformation
#> ℹ saving fit item: cholR
#> ℹ saving fit item: cholS
#> ℹ saving fit item: conditionNumberCor
#> ℹ saving fit item: conditionNumberCov
#> ℹ saving fit item: convergence
#> ℹ saving fit item: cov
#> ℹ saving fit item: covLvl
#> ℹ saving fit item: covMethod
#> ℹ saving fit item: covR
#> ℹ saving fit item: covRS
#> ℹ saving fit item: covS
#> ℹ saving fit item: eigenCor
#> ℹ saving fit item: eigenCov
#> ℹ saving fit item: eigenVecCor
#> ℹ saving fit item: eigenVecCov
#> ℹ saving fit item: est
#> ℹ saving fit item: etaObf
#> ℹ saving fit item: extra
#> ℹ saving fit item: fixef
#> ℹ saving fit item: foceiControl0
#> ℹ saving fit item: foceiModel
#> ℹ saving fit item: fullCor
#> ℹ saving fit item: iniDf0
#> ℹ saving fit item: llikObs
#> ℹ saving fit item: logLik
#> ℹ saving fit item: message
#> ℹ saving fit item: method
#> ℹ saving fit item: mixIdx
#> ℹ saving fit item: nAGQ
#> ℹ saving fit item: nEstOmega
#> ℹ saving fit item: nobs
#> ℹ saving fit item: nsub
#> ℹ saving fit item: objDf
#> ℹ saving fit item: objective
#> ℹ saving fit item: ofvType
#> ℹ saving fit item: omega
#> ℹ saving fit item: optReturn
#> ℹ saving fit item: origData
#> ℹ saving fit item: parFixed
#> ℹ saving fit item: parFixedDf
#> ℹ saving fit item: parHistData
#> ℹ saving fit item: phiC
#> ℹ saving fit item: phiH
#> ℹ saving fit item: qfirst
#> ℹ saving fit item: qw
#> ℹ saving fit item: qx
#> ℹ saving fit item: ranef
#> ℹ saving fit item: runInfo
#> ℹ saving fit item: scaleInfo
#> ℹ saving fit item: sessioninfo
#> ℹ saving fit item: shrink
#> ℹ saving fit item: table
#> ℹ saving fit item: time
#> ℹ saving fit item: tolFactor
#> ℹ saving fit item: ui
#> ℹ zipping fit files
#> ℹ removing unzipped fit files
#> ℹ loading fit from fit.R
#> lotri syntax error:
#> =================================================================================
#> :001: tka ~ c(tka = 0.0367813010669496)
#> :002: tcl ~ c(tka = -0.000782456725690811, tcl = 0.0069543545026029)
#> :003: tv ~ c(tka = 0.000952366834165392, tcl = -0.000489959598361193, tv = 0.00218330787936497)
#> :004: add.sd ~ c(tka = -6.41258631708858e-05, tcl = -5.67004131128066e-05, tv = 3.82936894597721e-05, add.sd = 0.00243929995816957)
#> lotri error:
#>    matrix expression should be 'name ~ c(lower-tri)'
#> :005: om.eta.ka ~ c(tka = 0.000146993075376898, tcl = 0.000126165231341427, tv = -0.000127191366433412, add.sd = -0.000541130240379496, om.eta.ka = 0.0353763038424305)
#> lotri error:
#>    number named variables and lower triangular matrix size do not match
#>      did you mean something like:
#>      'om.eta.cl + varName2 + varName3 ~ c(-0.000117433625253258, 
#>                                           0.000125380229779247, -6.81577112109223e-05, 
#>                                           9.82139979915425e-06, -0.00012172625636792, 0.00118026544368931)
#> :006: om.eta.cl ~ c(tka = -0.000117433625253258, tcl = 0.000125380229779247, tv = -6.81577112109223e-05, add.sd = 9.82139979915425e-06, om.eta.ka = -0.00012172625636792, om.eta.cl = 0.00118026544368931)
#> lotri error:
#>    matrix expression should be 'name ~ c(lower-tri)'
#> :007: om.eta.v ~ c(tka = 7.85881365155322e-05, tcl = -3.00853221214718e-05, tv = 5.01075513373757e-05, add.sd = -2.91997524119011e-05, om.eta.ka = 0.000117120738078429, om.eta.cl = -8.31373925204361e-05, om.eta.v = 0.000124134785919267)
#> =================================================================================
#> Error: lotri syntax errors above
# }
```
