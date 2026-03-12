
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nlmixr2save

<!-- badges: start -->

[![R-CMD-check](https://github.com/nlmixr2/nlmixr2save/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nlmixr2/nlmixr2save/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/nlmixr2/nlmixr2save/graph/badge.svg)](https://app.codecov.io/gh/nlmixr2/nlmixr2save)
<!-- badges: end -->

The goal of nlmixr2save is to save a nlmixr2 fit in file formats that
can be read outside R, avoiding `.rds` or other formats as much as
possible.

## Installation

You can install the development version of nlmixr2save from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("nlmixr2/nlmixr2save")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r

library(nlmixr2)
#> ── Attaching packages ───────────────────────────────────────── nlmixr2 5.0.0 ──
#> ✔ lotri         1.0.3          ✔ monolix2rx    0.0.6     
#> ✔ nlmixr2data   2.0.9          ✔ nlmixr2lib    0.3.2     
#> ✔ nlmixr2est    5.0.2.9000     ✔ nlmixr2rpt    0.2.2     
#> ✔ nlmixr2extra  5.0.0          ✔ nonmem2rx     0.1.9     
#> ✔ nlmixr2plot   5.0.1          ✔ posologyr     1.2.8     
#> ✔ rxode2        5.0.1.9000     ✔ shinyMixR     0.5.3     
#> ✔ babelmixr2    0.1.11         ✔ xpose.nlmixr2 0.4.1     
#> ✔ ggPMX         1.3.2
#> ── Optional Packages Loaded/Ignored ─────────────────────────── nlmixr2 5.0.0 ──
#> ✔ babelmixr2        ✔ nonmem2rx    
#> ✔ ggPMX             ✔ posologyr    
#> ✔ monolix2rx        ✔ shinyMixR    
#> ✔ nlmixr2lib        ✔ xpose.nlmixr2
#> ✔ nlmixr2rpt
#> ── Conflicts ───────────────────────────────────────────── nlmixr2conflicts() ──
#> ✖ rxode2::boxCox()     masks nlmixr2est::boxCox()
#> ✖ rxode2::yeoJohnson() masks nlmixr2est::yeoJohnson()
library(nlmixr2save) # eventually nlmixr2 will include this if available
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
#> calculating covariance matrix
#> [====|====|====|====|====|====|====|====|====|====] 0:00:00 
#> done

saveFit(fitF)
#> ℹ saving fit item: .rownum
#> ℹ saving fit item: AIC
#> ℹ saving fit item: aqHi
#> ℹ saving fit item: aqLow
#> ℹ saving fit item: aqn
#> ℹ saving fit item: BIC
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
#> ℹ saving fit item: R
#> ℹ saving fit item: R.0
#> ℹ saving fit item: R.E
#> ℹ saving fit item: R.pd
#> ℹ saving fit item: ranef
#> ℹ saving fit item: Rinv
#> ℹ saving fit item: runInfo
#> ℹ saving fit item: S
#> ℹ saving fit item: S.E
#> ℹ saving fit item: S.pd
#> ℹ saving fit item: S0
#> ℹ saving fit item: scaleInfo
#> ℹ saving fit item: sessioninfo
#> ℹ saving fit item: shrink
#> ℹ saving fit item: Sper
#> ℹ saving fit item: table
#> ℹ saving fit item: time
#> ℹ saving fit item: ui
#> ℹ zipping fit files
#> ℹ removing unzipped fit files

# You can also load this with `loadFit(fitF)`

print(loadFit(fitF))
#> ℹ loading fit from fitF.R
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> ℹ removing unzipped fit files
#> ── nlmixr² FOCEi (outer: nlminb) ──
#> 
#>           OBJF      AIC      BIC Log-likelihood Condition#(Cov) Condition#(Cor)
#> FOCEi 116.8044 373.4041 393.5838      -179.7021        68.53386        9.375135
#> 
#> ── Time (sec $time): ──
#> 
#>            setup optimize covariance table compress    other
#> elapsed 0.002021 0.252837   0.252838 0.077    0.001 2.668304
#> 
#> ── Population Parameters ($parFixed or $parFixedDf): ──
#> 
#>        Parameter  Est.     SE %RSE Back-transformed(95%CI) BSV(CV%) Shrink(SD)%
#> tka              0.463  0.195 42.2       1.59 (1.08, 2.33)     70.7      2.03% 
#> tcl               1.01 0.0751 7.42       2.75 (2.38, 3.19)     26.7      3.89% 
#> tv         log V  3.46 0.0436 1.26       31.8 (29.2, 34.7)     14.0      10.5% 
#> add.sd           0.694                               0.694                     
#>  
#>   Covariance Type ($covMethod): r,s
#>   Some strong fixed parameter correlations exist ($cor) :
#>     cor:tcl,tka  cor:tv,tka  cor:tv,tcl 
#>      0.158       0.421       0.744  
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
#> 2     1  0.25  2.84  3.26 -0.419 -0.505  3.85 -1.01  -1.45   3.49 -0.652 -0.744
#> 3     1  0.57  6.57  5.82  0.746  0.699  6.78 -0.215 -0.310  6.16  0.409  0.349
#> # ℹ 129 more rows
#> # ℹ 10 more variables: eta.ka <dbl>, eta.cl <dbl>, eta.v <dbl>, depot <dbl>,
#> #   central <dbl>, ka <dbl>, cl <dbl>, v <dbl>, tad <dbl>, dosenum <int>
```

This creates a “.zip” file that includes a “fitF.R” file. This fit
builds the fit from (mostly) csv and R source files.

The reason for this extra package saving models:

- The model properties and results can be examined outside of R

- This model can be saved without worrying how data are serialized
  and/or compressed

- This saved model version **should** be independent of the version of
  `rxode2` and `nlmixr2`, whereas other methods of saving are dependent
  on the version of nlmixr2/rxode2

In the future, this could be used in conjunction with `vroom`, `altrep`
and other similar tools to partially load fit without loading the
corresponding datasets.

In addition to introducing the R-independent zip file for nlmixr2 fits,
this also introduced the cache assignment operator `:=` which will cache
functions calls when assigning them.

In the case of `nlmixr2` fits, these cached values will be the zip files
described above; Otherwise, this will be a `rds` file that has the
object and the md5 of the arguments in a list.

If the object is NOT a function, it cannot load the cache, but may have
some side effects.

In the case of a nlmixr2 fit, you could assign it as follows

``` r
fit2 := fit # The fit2.zip is created to save the fit
```

Here are some annotated examples of the cache assignment operator in
pratice:

``` r
# You can also automatically load a fit if the arguments for nlmixr2 do not change
# by using the := cached assignment operator introduced in this package

fitF := nlmixr(one.cmt, theo_sd, est="focei", control=list(print=0, compress=FALSE))
#> ℹ loading fit from fitF.R
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> ℹ removing unzipped fit files
#> ℹ fit in fitF.zip does not match current fit; removing and refitting
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> calculating covariance matrix
#> [====|====|====|====|====|====|====|====|====|====done
#> → Calculating residuals/tables
#> ✔ done
#> ℹ saving fit item: .rownum
#> ℹ saving fit item: AIC
#> ℹ saving fit item: aqHi
#> ℹ saving fit item: aqLow
#> ℹ saving fit item: aqn
#> ℹ saving fit item: BIC
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
#> ℹ saving fit item: nlmixr2save
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
#> ℹ saving fit item: R
#> ℹ saving fit item: R.0
#> ℹ saving fit item: R.E
#> ℹ saving fit item: R.pd
#> ℹ saving fit item: ranef
#> ℹ saving fit item: Rinv
#> ℹ saving fit item: runInfo
#> ℹ saving fit item: S
#> ℹ saving fit item: S.E
#> ℹ saving fit item: S.pd
#> ℹ saving fit item: S0
#> ℹ saving fit item: scaleInfo
#> ℹ saving fit item: sessioninfo
#> ℹ saving fit item: shrink
#> ℹ saving fit item: Sper
#> ℹ saving fit item: table
#> ℹ saving fit item: time
#> ℹ saving fit item: ui
#> ℹ zipping fit files
#> ℹ removing unzipped fit files

# Now using the same arguments it loads
fitF := nlmixr(one.cmt, theo_sd, est="focei", control=list(print=0, compress=FALSE))
#> ℹ loading fit from fitF.R
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> ℹ removing unzipped fit files

# But if we use different arguments, say using saem, it will rerun.
fitF := nlmixr(one.cmt, theo_sd, est="saem", control=list(print=0, compress=FALSE))
#> ℹ loading fit from fitF.R
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> ℹ removing unzipped fit files
#> ℹ fit in fitF.zip does not match current fit; removing and refitting
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> → loading into symengine environment...
#> → pruning branches (`if`/`else`) of saem model...
#> ✔ done
#> → finding duplicate expressions in saem model...
#> [====|====|====|====|====|====|====|====|====|====
#> ====|====|====|====|====|====|====|====|====|====] 0:00:00
#> ✔ done
#> ℹ calculate uninformed etas
#> ℹ done
#> Calculating covariance matrix
#> [====|====|====|====|====|====|====|====|====|====] 0:00:00
#> → loading into symengine environment...
#> → pruning branches (`if`/`else`) of saem model...
#> ✔ done
#> → finding duplicate expressions in saem predOnly model 0...
#> [====|====|====|====|====|====|====|====|====|====] 0:00:00
#> → finding duplicate expressions in saem predOnly model 1...
#> [====|====|====|====|====|====|====|====|====|====] 0:00:00
#> → finding duplicate expressions in saem predOnly model 2...
#> [====|====|====|====|====|====|====|====|====|====] 0:00:00
#> ✔ done
#> → Calculating residuals/tables
#> ✔ done
#> ℹ saving fit item: .likTime
#> ℹ saving fit item: .rownum
#> ℹ saving fit item: adjObf
#> ℹ saving fit item: AIC
#> ℹ saving fit item: aqHi
#> ℹ saving fit item: aqLow
#> ℹ saving fit item: aqn
#> ℹ saving fit item: BIC
#> ℹ saving fit item: censInformation
#> ℹ saving fit item: conditionNumberCor
#> ℹ saving fit item: conditionNumberCov
#> ℹ saving fit item: cov
#> ℹ saving fit item: covLvl
#> ℹ saving fit item: covMethod
#> ℹ saving fit item: eigenCor
#> ℹ saving fit item: eigenCov
#> ℹ saving fit item: eigenVecCor
#> ℹ saving fit item: eigenVecCov
#> ℹ saving fit item: est
#> ℹ saving fit item: etaObf
#> ℹ saving fit item: extra
#> ℹ saving fit item: fixef
#> ℹ saving fit item: foceiControl0
#> ℹ saving fit item: fullCor
#> ℹ saving fit item: iniDf0
#> ℹ saving fit item: logLik
#> ℹ saving fit item: message
#> ℹ saving fit item: method
#> ℹ saving fit item: mixIdx
#> ℹ saving fit item: nAGQ
#> ℹ saving fit item: nEstOmega
#> ℹ saving fit item: nlmixr2save
#> ℹ saving fit item: nobs
#> ℹ saving fit item: nsub
#> ℹ saving fit item: objDf
#> ℹ saving fit item: objective
#> ℹ saving fit item: ofvType
#> ℹ saving fit item: omega
#> ℹ saving fit item: origData
#> ℹ saving fit item: parFixed
#> ℹ saving fit item: parFixedDf
#> ℹ saving fit item: parHistData
#> ℹ saving fit item: phiM
#> ℹ saving fit item: qfirst
#> ℹ saving fit item: qw
#> ℹ saving fit item: qx
#> ℹ saving fit item: ranef
#> ℹ saving fit item: runInfo
#> ℹ saving fit item: saem0
#> ℹ saving fit item: saemControl
#> ℹ saving fit item: saemModel
#> ℹ saving fit item: scaleInfo
#> ℹ saving fit item: sessioninfo
#> ℹ saving fit item: shrink
#> ℹ saving fit item: table
#> ℹ saving fit item: time
#> ℹ saving fit item: ui
#> ℹ zipping fit files
#> ℹ removing unzipped fit files

# note this approach does not evaluate the first function only,
# everything else is evaluated so `suppressMessage(nlmixr(...))` would always run
# the model, never loading the cached model.

# The cached assignment operator works for generic functions too, but saves
# as a rds file.  For example,

sim := rxSolve(one.cmt, theo_sd) # First simulate
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments

sim := rxSolve(one.cmt, theo_sd) # Same arguments, load instead
#> ℹ loading fit from sim.rds
#> ℹ loading from sim.rds

sim := rxSolve(one.cmt, theo_sd, nStud=4) # Different arguments, resimulate
#> ℹ loading fit from sim.rds
#> ℹ fit in sim.rds does not match argument md5, removing and re-running command
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
```
