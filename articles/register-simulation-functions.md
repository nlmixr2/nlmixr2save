# Registering simulation functions

`nlmixr2save` can cache more than `nlmixr2` fits. If your package has
its own simulation or stochastic estimation functions, you can register
them so the `:=` operator restores cached results without breaking
reproducibility.

## Registering a stochastic simulation function

Use [`saveFitRandom()`](../reference/saveFitRandom.md) to tell
`nlmixr2save` that a function should be treated as random-state-aware.

``` r

library(nlmixr2save)

saveFitRandom("mySimulate")
```

After registration, calls of the form below are cached in `sim.rds`
together with the starting and ending random state:

``` r

sim := mySimulate(model, n = 1000)
```

On restore, `nlmixr2save` checks:

1.  the function arguments,
2.  whether the original run changed the random state, and
3.  whether the current run starts from the same random state.

If those checks pass, the cached value is loaded and the random seed is
advanced to the same state the original run would have produced.

## Registering at package load time

If your package always wants a function treated as stochastic, register
it in `.onLoad()`:

``` r

.onLoad <- function(libname, pkgname) {
  nlmixr2save::saveFitRandom("mySimulate")
}
```

You can also unregister it later:

``` r

saveFitRandom("mySimulate", remove = TRUE)
```

## Integrating stochastic estimation methods

`nlmixr2save` handles `nlmixr2(...)` calls differently from ordinary
function calls:

- deterministic estimation methods are cached as saved fits (`.zip`
  bundles),

- stochastic estimation methods are cached with seed metadata (`.rds`
  files).

To place your estimation method into the seed-aware group, define the
`nlmixr2Est.<method>` method with a `random` attribute set to `TRUE` or
to a function that inspects the control object.

``` r

nlmixr2Est.myMethod <- function(object, data, control, ...) {
  # run stochastic estimation here
}

attr(nlmixr2Est.myMethod, "random") <- TRUE
```

If the stochastic behavior depends on control settings, the attribute
can be a function:

``` r

attr(nlmixr2Est.myMethod, "random") <- function(control) {
  isTRUE(control$stochastic)
}
```

With that in place, this call uses the seed-aware cache path
automatically:

``` r

fit := nlmixr2(model, data, est = "myMethod", control = list(stochastic = TRUE))
```

## When to use each integration path

Use [`saveFitRandom()`](../reference/saveFitRandom.md) when:

1.  you are caching a regular function such as a simulation helper,

2.  the result should be stored as a generic `.rds`, and

3.  reproducibility depends on restoring the random seed state.

Use the `random` attribute on `nlmixr2Est.<method>` when:

1.  the function is an `nlmixr2` estimation method,

2.  deterministic runs should still use the fit-saving `.zip` path, and

3.  stochastic runs should move into the seed-aware `.rds` path.

## Practical limitations

The same limitations apply here as in the main usage vignette:

1.  `:=` needs to see the expensive call directly on the right-hand
    side.

2.  Seed-aware restores only replay safely when the starting random
    state matches.

3.  Generic registered functions are restored from `.rds`, not from the
    text-and-csv fit format used for deterministic `nlmixr2` fits.
