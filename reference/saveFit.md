# Save a fitted model object to a series of files

Save a fitted model object to a series of files

## Usage

``` r
saveFit(fit, file, zip = TRUE)

# S3 method for class 'nlmixr2FitCore'
saveFit(fit, file, zip = TRUE)

# S3 method for class 'nlmixr2FitData'
saveFit(fit, file, zip = TRUE)

# Default S3 method
saveFit(fit, file, zip = TRUE)
```

## Arguments

- fit:

  the fitted model object

- file:

  the base name of the files to save the fit to.

- zip:

  Boolean indicating if the files should be zipped.

## Value

nothing, called for side effects

## Author

Matthew L. Fidler

## Examples

``` r
# \donttest{
  if (requireNamespace("nlmixr2est", quietly=TRUE) && requireNamespace("withr")) {
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
#> ── nlmixr² FOCEi (outer: nlminb) ──
#> 
#>           OBJF      AIC      BIC Log-likelihood Condition#(Cov) Condition#(Cor)
#> FOCEi 116.8044 373.4042 393.5838      -179.7021         68.7787        9.398479
#> 
#> ── Time (sec $time): ──
#> 
#>            setup optimize covariance table compress    other
#> elapsed 0.001872 0.388862   0.388863 0.048    0.002 1.285403
#> 
#> ── Population Parameters ($parFixed or $parFixedDf): ──
#> 
#>         Est.     SE %RSE Back-transformed(95%CI) BSV(CV%) Shrink(SD)%
#> tka    0.464  0.195 42.1       1.59 (1.08, 2.33)     70.6      1.94% 
#> tcl     1.01 0.0751 7.42       2.75 (2.38, 3.19)     26.8      3.96% 
#> tv      3.46 0.0435 1.26       31.8 (29.2, 34.6)     13.9      10.3% 
#> add.sd 0.693                               0.693                     
#>  
#>   Covariance Type ($covMethod): r,s
#>   Some strong fixed parameter correlations exist ($cor) :
#>     cor:tcl,tka  cor:tv,tka  cor:tv,tcl 
#>      0.157        0.422       0.744  
#>  
#> 
#>   No correlations in between subject variability (BSV) matrix
#>   Full BSV covariance ($omega) or correlation ($omegaR; diagonals=SDs) 
#>   Distribution stats (mean/skewness/kurtosis/p-value) available in $shrink 
#>   Information about run found ($runInfo):
#>    • gradient problems with initial estimate and covariance; see $scaleInfo 
#>    • ETAs were reset to zero during optimization; (Can control by foceiControl(resetEtaP=.)) 
#>    • initial ETAs were nudged; (can control by foceiControl(etaNudge=., etaNudge2=)) 
#>   Censoring ($censInformation): No censoring
#>   Minimization message ($message):  
#>     relative convergence (4) 
#> 
#> ── Fit Data (object is a modified tibble): ──
#> # A tibble: 132 × 22
#>      ID  TIME    DV  PRED    RES   WRES IPRED   IRES  IWRES CPRED   CRES  CWRES
#>   <int> <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl>  <dbl>  <dbl> <dbl>  <dbl>  <dbl>
#> 1     1  0     0.74  0     0.74   1.07   0     0.74   1.07   0     0.74   1.07 
#> 2     1  0.25  2.84  3.26 -0.424 -0.513  3.85 -1.01  -1.45   3.50 -0.657 -0.752
#> 3     1  0.57  6.57  5.83  0.738  0.694  6.79 -0.215 -0.310  6.17  0.400  0.343
#> # ℹ 129 more rows
#> # ℹ 10 more variables: eta.ka <dbl>, eta.cl <dbl>, eta.v <dbl>, depot <dbl>,
#> #   central <dbl>, ka <dbl>, cl <dbl>, v <dbl>, tad <dbl>, dosenum <int>
# }
```
