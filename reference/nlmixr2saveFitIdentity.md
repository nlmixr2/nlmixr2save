# Identity and cache keys of an nlmixr2 fit

`nlmixr2saveFitIdentity()` identifies *which* fit this is: the model,
the estimation method, the estimates and the data. It is the same for a
fit, its [`saveFit()`](saveFit.md)/[`loadFit()`](loadFit.md) reload, and
the fit after
[`addCwres()`](https://nlmixr2.github.io/nlmixr2est/reference/addCwres.html).

## Usage

``` r
nlmixr2saveFitIdentity(fit)

nlmixr2saveFitKey(fit, sha = TRUE)
```

## Arguments

- fit:

  An nlmixr2 fit.

- sha:

  Include nlmixr2save's own `:=` fields (`nlmixr2save`,
  `nlmixr2saveOrig`)?

## Value

A list suitable for hashing.

## Details

`nlmixr2saveFitKey()` additionally changes whenever something a
downstream result could depend on changes: added table columns
([`addCwres()`](https://nlmixr2.github.io/nlmixr2est/reference/addCwres.html),
[`addNpde()`](https://nlmixr2.github.io/nlmixr2est/reference/addNpde.html),
[`addTable()`](https://nlmixr2.github.io/nlmixr2est/reference/addTable.html)),
objective function rows
([`setOfv()`](https://nlmixr2.github.io/nlmixr2est/reference/setOfv.html)),
the covariance (`preconditionFit()`) or the table values. `:=` hashes
fits on its right-hand side with this key, so a result built on a fit
reloaded in a new session is still a cache hit.

Neither key forces or writes anything in the fit.

## Author

Matthew L. Fidler
