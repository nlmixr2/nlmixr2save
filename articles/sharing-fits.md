# Sharing a fit without the original data

A saved `nlmixr2` fit normally travels with the data it was fit to: the
subject-level dataset is stored inside the `.zip` (as `origData`) and,
for a standard fit, so is the returned per-observation
prediction/residual table. That is convenient for your own re-use, but
it is often exactly what you cannot share – the data may be
confidential, licensed, large, or simply not yours to distribute.

`nlmixr2save` lets you export a fit **without the original data**, and
optionally **without the output tables** as well.

## Omitting the data when you save

[`saveFit()`](../reference/saveFit.md) takes a `data` argument. With
`data = FALSE`, the original dataset is left out of the zip:

``` r

library(nlmixr2save)

saveFit(fit, data = FALSE) # writes fit.zip with no origData inside
```

To make that the default for a whole script or report, set the option:

``` r

options(nlmixr2save.data = FALSE)
```

The saved fit is otherwise complete: it still contains the model, the
parameter estimates, and (for a standard fit) the prediction/residual
columns. Only the input dataset is gone.

## Sharing an existing saved fit: `nlmixr2saveShare()`

If you already have `fit.zip` (with data), you do not need to re-run
anything. [`nlmixr2saveShare()`](../reference/nlmixr2saveShare.md) reads
it and writes a stripped sibling zip, leaving the original untouched:

``` r

# fit.zip  ->  fit-noData.zip  (data removed, predictions/tables kept)
nlmixr2saveShare("fit")

# fit.zip  ->  fit-noData-noFit.zip  (data AND output tables removed)
nlmixr2saveShare("fit", noFit = TRUE)
```

[`nlmixr2saveShare()`](../reference/nlmixr2saveShare.md) also accepts a
live fit object (it takes the output name from the object), so you can
share straight from a session:

``` r

nlmixr2saveShare(fit)              # -> fit-noData.zip
nlmixr2saveShare(fit, noFit = TRUE) # -> fit-noData-noFit.zip
```

It resolves file names through the same `nlmixr2save.dir` and
`nlmixr2save.prefix` options as the `:=` cache, so it finds prefixed
caches in the cache directory and writes the shareable copies alongside
them:

``` r

options(nlmixr2save.dir = "cache", nlmixr2save.prefix = "mp-")
nlmixr2saveShare("fit") # reads cache/mp-fit.zip -> writes cache/mp-fit-noData.zip
```

### What `noFit` removes

[`nlmixr2saveShare()`](../reference/nlmixr2saveShare.md) always removes
the original data. With `noFit = TRUE` it *additionally* removes the
returned prediction/residual data frame, so the shared fit is reduced to
the model and its results. Internally this saves the fit’s **core** (the
same shape a `calcTables = FALSE` fit already has) rather than the data
frame:

| kept / removed | `fit-noData.zip` | `fit-noData-noFit.zip` |
|:---|:--:|:--:|
| model, `iniDf`, `parFixed`, `objDf`, `omega` | kept | kept |
| eta table (`etaObf`), parameter history (`parHistData`) | kept | kept |
| per-observation prediction/residual data frame | kept | **removed** |
| original dataset (`origData`) | **removed** | **removed** |

## Side effects – what a stripped fit can no longer do

Removing data is not free. The stripped fit still loads, prints, and
reports its parameter estimates, but anything that needs what you
removed will not work.

**Without the original data (`-noData`, or `saveFit(data = FALSE)`):**

- `fit$origData` is absent.
- Operations that need the raw dataset fail or are degraded: visual
  predictive checks
  ([`vpcSim()`](https://nlmixr2.github.io/nlmixr2est/reference/vpcSim.html)
  / tidyvpc), re-deriving residuals or predictions
  ([`augPred()`](https://rdrr.io/pkg/nlme/man/augPred.html),
  [`addCwres()`](https://nlmixr2.github.io/nlmixr2est/reference/addCwres.html),
  [`addNpde()`](https://nlmixr2.github.io/nlmixr2est/reference/addNpde.html),
  …), and re-fitting or updating the model.
- The prediction/residual columns already computed in the fit (IPRED,
  PRED, CWRES, …) and the parameter tables are unaffected, so printing
  and standard goodness-of-fit columns still work.

**Without the output tables (`-noData-noFit`, `noFit = TRUE`):**

- The fit loads as a `nlmixr2FitCore` (an environment), **not** a
  `nlmixr2FitData` data frame – there is no per-observation table, so
  `as.data.frame(fit)` has no rows and plots/diagnostics that need those
  rows will not work.
- The model, parameter estimates (`parFixed` / `parFixedDf`), objective
  (`objDf`), covariance (`omega`), eta table (`etaObf`), and parameter
  history (`parHistData`) remain – enough to inspect and report the
  fitted model.
- This form has no original data either (it is a superset of `-noData`).

## Note on the cache operator

The `data` option affects [`saveFit()`](../reference/saveFit.md) and
[`nlmixr2saveShare()`](../reference/nlmixr2saveShare.md) only. The `:=`
caching operator is intentionally left unchanged: it re-attaches the
data from the live call on restore, so its cache continues to behave as
before.
