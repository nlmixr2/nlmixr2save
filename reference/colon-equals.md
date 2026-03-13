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
  does not exist or if the md5 hash of the arguments does not match).

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
running the possibly expensive fitting process (as long as the md5 hash
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
    # First fit creates fit.zip
    fit := nlmixr2(one.cmt, theo_sd, est="focei")

    # Second fit loads from fit.zip since it had the same options
    fit := nlmixr2(one.cmt, theo_sd, est="focei")

    # Third fit refits since the options are different
    fit := nlmixr2(one.cmt, theo_sd, est="saem")
  })
 }
#> Loading required package: nlmixr2data
#> Error in .nlmixr2saveEst(one.cmt, theo_sd, est = "focei"): could not find function ".nlmixr2saveEst"
# }
```
