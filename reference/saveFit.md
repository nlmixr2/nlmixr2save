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
#>  
#>  
#>  
#>  
#>  
#>  
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> ℹ removing unzipped fit files
#> ── nlmixr² FOCEi (outer: bobyqa) ──
#> 
#>           OBJF      AIC      BIC Log-likelihood Condition#(Cov) Condition#(Cor)
#> FOCEi 116.8037 373.4035 393.5831      -179.7017         2730.79         89.2609
#> 
#> ── Time (sec $time): ──
#> 
#>             setup optimize covariance preprocess postprocess table compress
#> elapsed 0.1208009  0.19011  0.5376478      0.026       0.014 0.045    0.001
#>             other
#> elapsed 0.1294413
#> 
#> ── Population Parameters ($parFixed or $parFixedDf): ──
#> 
#>         Est.     SE %RSE Back-transformed(95%CI) BSV(CV%) Shrink(SD)%
#> tka    0.465  0.198 42.5       1.59 (1.08, 2.34)     70.4       1.80 
#> tcl     1.01 0.0755 7.45       2.75 (2.37, 3.19)     26.7       3.77 
#> tv      3.46 0.0429 1.24       31.8 (29.3, 34.6)     13.9       10.4 
#> add.sd 0.694 0.0924 13.3    0.694 (0.513, 0.875)                     
#>  
#>   Covariance Type ($covMethod): r,s
#>   Some strong fixed parameter correlations exist ($cor) :
#>                 cor:tcl,tka              cor:tv,tka          cor:add.sd,tka 
#>                  0.196                    0.400                 -0.0752   
#>       cor:om.eta.ka,tka       cor:om.eta.cl,tka        cor:om.eta.v,tka 
#>                  0.579                  -0.139                   -0.289   
#>              cor:tv,tcl          cor:add.sd,tcl       cor:om.eta.ka,tcl 
#>                  0.805                  -0.219                    0.128   
#>       cor:om.eta.cl,tcl        cor:om.eta.v,tcl           cor:add.sd,tv 
#>                 -0.476                   0.355                  -0.352  
#>        cor:om.eta.ka,tv        cor:om.eta.cl,tv         cor:om.eta.v,tv 
#>                  0.113                  -0.0451                   0.0892   
#>    cor:om.eta.ka,add.sd    cor:om.eta.cl,add.sd     cor:om.eta.v,add.sd 
#>                 -0.457                  -0.110                   -0.331  
#> cor:om.eta.cl,om.eta.ka  cor:om.eta.v,om.eta.ka  cor:om.eta.v,om.eta.cl 
#>                 -0.198                   0.0573                  -0.0682   
#>  
#> 
#>   No correlations in between subject variability (BSV) matrix
#>   Full BSV covariance ($omega) or correlation ($omegaR; diagonals=SDs) 
#>   Distribution stats (mean/skewness/kurtosis/p-value) available in $shrink 
#>   Information about run found ($runInfo):
#>    • gradient problems with covariance; see $scaleInfo 
#>    • last objective function was not at minimum, possible problems in optimization 
#>    • ETAs were reset to zero during optimization; (Can control by foceiControl(resetEtaP=.)) 
#>   Censoring ($censInformation): No censoring
#>   Minimization message ($message):  
#>     Normal exit from bobyqa 
#> 
#> ── Fit Data (object is a modified tibble): ──
#> # A tibble: 132 × 22
#>      ID  TIME    DV  PRED    RES   WRES IPRED   IRES  IWRES CPRED   CRES  CWRES
#>   <int> <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl>  <dbl>  <dbl> <dbl>  <dbl>  <dbl>
#> 1     1  0     0.74  0     0.74   1.07   0     0.74   1.07   0     0.74   1.07 
#> 2     1  0.25  2.84  3.26 -0.423 -0.226  3.85 -1.01  -1.45   3.22 -0.379 -0.178
#> 3     1  0.57  6.57  5.83  0.740  0.297  6.79 -0.215 -0.310  5.77  0.796  0.288
#> # ℹ 129 more rows
#> # ℹ 10 more variables: eta.ka <dbl>, eta.cl <dbl>, eta.v <dbl>, depot <dbl>,
#> #   central <dbl>, ka <dbl>, cl <dbl>, v <dbl>, tad <dbl>, dosenum <int>
# }
```
