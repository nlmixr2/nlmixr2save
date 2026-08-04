# This assignment operator is meant to assign or load a nlmixr2 fit (and other objects)

By default it is equivalent to the standard assignment operator `<-`,
but it is a S3 generic so it can have other behaviors for specific
classes.

## Arguments

- x:

  the name of the object to assign the value to

- value:

  the value to assign to the object, because R can use non-standard
  evaluation, this expression may not be evaluated when passed to the
  function. In the case of the `nlmixr2` function, the expression will
  be evaluated only if the fit needs to be refit (i.e. if the zip file
  does not exist or if the sha1 hash of the arguments does not match).

## Value

the value that was assigned to the object, invisibly. It also has the
side effect of assigning the value to the parent environment.

## Details

For example, when used with a nlmixr2 call, say:

fit := nlmixr2(one.cmt, theo_sd, est="focei")

the `:=` operator will assign the result of the `nlmixr2` call to `fit`,
but it will also save the fit to a file named "fit.zip" in the current
working directory.

If the "fit.zip" file already exists, it will be loaded instead of
running the possibly expensive fitting process (as long as the sha1 hash
of the arguments are the same).

This allows for easy saving and loading of fitted models without having
to explicitly call a save function.

This S3 generic can be extended to other classes as needed, allowing for
custom behaviors when assigning values to objects of those classes.

When trying to save expensive evaluations like the output of a
[`nlmixr2()`](https://nlmixr2.github.io/nlmixr2est/reference/nlmixr2.html)
fit, the s3 dispach would be to `:=.assign_nlmixr2(x, value)` or
whatever function is used in the call. This allows checking the
arguments to see if there can be a cache that will be loaded.

Otherwise, the default s3 method would be `:=.class` where `class`
instead. Unlike the un-evaluated function dispach there is no way to
check the arguments for a cache, so loading from cache is not possible.

## See also

[`saveFit()`](saveFit.md) for saving fitted model objects to files,
[`loadFit()`](loadFit.md) for loading fitted model objects from files,
and [`.assignParent()`](dot-assignParent.md) for getting or setting the
environment used in the `:=` operator.

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
    # First fit creates fit.zip
    fit := nlmixr2(one.cmt, theo_sd, est="focei")

    # Second fit loads from fit.zip since it had the same options
    fit := nlmixr2(one.cmt, theo_sd, est="focei")

    # Third fit refits since the options are different
    fit := nlmixr2(one.cmt, theo_sd, est="saem")
  })
 }
#> Loading required package: nlmixr2data
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#>  
#>  
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> → loading into symengine environment...
#> → pruning branches (`if`/`else`) of full model...
#> ✔ done
#> → calculate ∂(f)/∂(η)
#> → calculate ∂(R²)/∂(η)
#> → finding duplicate expressions in inner model...
#> → optimizing duplicate expressions in inner model...
#> → finding duplicate expressions in EBE model...
#> → optimizing duplicate expressions in EBE model...
#> → compiling inner model...
#>  
#>  
#> ✔ done
#> → finding duplicate expressions in FD model...
#> → compiling EBE model...
#>  
#>  
#> ✔ done
#> → compiling events FD model...
#>  
#>  
#> ✔ done
#> rxode2 5.1.6 using 2 threads (see ?getRxThreads)
#>   no cache: create with `rxCreateCache()`
#> 
#> Attaching package: ‘rxode2’
#> The following objects are masked from ‘package:nlmixr2est’:
#> 
#>     boxCox, yeoJohnson
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
#> ℹ saving fit item: nlmixr2save
#> ℹ saving fit item: nlmixr2saveOrig
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
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
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
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
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
#> ℹ fit in fit.zip does not match current fit; removing and refitting
#>  
#>  
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#>  
#>  
#> → loading into symengine environment...
#> → pruning branches (`if`/`else`) of saem model...
#> ✔ done
#> → finding duplicate expressions in saem model...
#> ✔ done
#> ℹ calculate uninformed etas
#> ℹ done
#> covMethod="sa" could not be computed; using the linearized FIM
#> Calculating covariance matrix
#> → loading into symengine environment...
#> → pruning branches (`if`/`else`) of saem model...
#> ✔ done
#> → finding duplicate expressions in saem predOnly model 0...
#> → finding duplicate expressions in saem predOnly model 1...
#> → finding duplicate expressions in saem predOnly model 2...
#> → optimizing duplicate expressions in saem predOnly model 2...
#> ✔ done
#>  
#>  
#> → Calculating residuals/tables
#> ✔ done
#> → compress origData in nlmixr2 object, save 6584
#> → compress parHistData in nlmixr2 object, save 8800
#> → compress phiM in nlmixr2 object, save 446912
#> ℹ saving fit item: .likTime
#> ℹ saving fit item: .rownum
#> ℹ saving fit item: .saemFullCov
#> ℹ saving fit item: AIC
#> ℹ saving fit item: BIC
#> ℹ saving fit item: adjObf
#> ℹ saving fit item: aqHi
#> ℹ saving fit item: aqLow
#> ℹ saving fit item: aqn
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
#> ℹ saving fit item: logThetasF
#> ℹ saving fit item: logitThetasF
#> ℹ saving fit item: logitThetasHiF
#> ℹ saving fit item: logitThetasLowF
#> ℹ saving fit item: message
#> ℹ saving fit item: method
#> ℹ saving fit item: mixIdx
#> ℹ saving fit item: nAGQ
#> ℹ saving fit item: nEstOmega
#> ℹ saving fit item: nlmixr2save
#> ℹ saving fit item: nlmixr2saveOrig
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
#> ℹ saving fit item: probitThetasF
#> ℹ saving fit item: probitThetasHiF
#> ℹ saving fit item: probitThetasLowF
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
#> ℹ saving fit item: tolFactor
#> ℹ saving fit item: ui
#> ℹ zipping fit files
#> ℹ removing unzipped fit files
# }
```
