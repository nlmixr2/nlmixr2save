# nlmixr2save events

`nlmixr2save` tells loggers *what was assigned*: when
`fit := nlmixr2(...)` finishes, a logger learns that the result was
named `fit`, whether it came from the cache, and the hash it was stored
under. It does this by reporting to the `rxode2` event bus.

## The event bus in one paragraph

`rxode2` provides a small publish/subscribe mechanism: packages call
`rxode2::rxEventEmit(event, ...)` when a top-level operation finishes,
and any function registered with `rxode2::rxEventListen(id, fun)` is
called as `fun(event, ...)` with the payload fields as named arguments.
Events are only delivered from the outermost operation (scope depth 0),
so the fits inside a bootstrap or the solves inside a fit are silent.
See
[`?rxode2::rxEventListen`](https://nlmixr2.github.io/rxode2/reference/rxEventListen.html)
for the full API (`rxEventScope()`, `rxEventDepth()`, `rxEventSeq()`,
…).

Other packages emit the events that describe the *work* (`fitComplete`
from
[`nlmixr2est::nlmixr2()`](https://nlmixr2.github.io/nlmixr2est/reference/nlmixr2.html),
`solveComplete` from
[`rxode2::rxSolve()`](https://nlmixr2.github.io/rxode2/reference/rxSolve.html),
…). `nlmixr2save` emits the events that describe the *assignment* around
that work. Every emitter checks that the bus exists first, so with an
older `rxode2` nothing is emitted and nothing breaks.

## Events emitted by nlmixr2save

| Function       | Events                                        |
|----------------|-----------------------------------------------|
| `x := value`   | `assignStart`, then `assign` or `assignAbort` |
| `saveFit(fit)` | `assign` (only outside `:=`, only for a fit)  |

### `assignStart`

Emitted by `:=` before anything is evaluated or loaded, so a logger can
attribute every event that follows (a `fitComplete`, a `solveComplete`)
to the name being assigned.

| Field  | Meaning                                                     |
|--------|-------------------------------------------------------------|
| `name` | The name on the left of `:=` (`NULL` if it is not a symbol) |

### `assign`

Emitted by `:=` when it returns successfully, whether the value was
computed or restored from the cache.

| Field | Meaning |
|----|----|
| `name` | The name assigned (same as in `assignStart`) |
| `value` | The value assigned (a fit, a simulation, a data frame, …) |
| `call` | The right-hand side of `:=`, normalized by `rxode2` so it never embeds large values |
| `envir` | The environment the name was assigned in |
| `cached` | `TRUE` when the value was restored from disk instead of computed |
| `sha` | The hash the value is cached under (`NULL` in trusted-cache mode, `options(nlmixr2save.check = FALSE)`) |
| `random` | `TRUE` when the right-hand side is a random/simulation function ([`rxSolve()`](https://nlmixr2.github.io/rxode2/reference/rxSolve.html), [`vpcSim()`](https://nlmixr2.github.io/nlmixr2est/reference/vpcSim.html), …) |
| `seed` | For a random result, the random seed captured before it ran; `NULL` otherwise |
| `since` | [`rxode2::rxEventSeq()`](https://nlmixr2.github.io/rxode2/reference/rxEventListen.html) just after `assignStart`; events delivered after it belong to this assignment |

`since` lets a logger collect the events produced by the right-hand
side: every event whose sequence number is greater than `since` and that
was delivered before this `assign` belongs to it.

### `assignAbort`

Emitted by `:=` when the right-hand side (or the saving) errors. The
only field is `name`. A logger can drop whatever it collected since the
matching `assignStart`.

### `assign` from `saveFit()`

A direct [`saveFit()`](../reference/saveFit.md) of an `nlmixr2` fit
reports an `assign` after the fit is written, with the same fields:

- `name` is the `file` argument when given, otherwise the name of the
  variable passed as `fit` (`saveFit(fit)` reports `"fit"`);
- `cached = FALSE`, `random = FALSE`, and `sha`, `seed` and `since` are
  `NULL`;
- there is no `assignStart` and no `assignAbort`: a
  [`saveFit()`](../reference/saveFit.md) that errors emits nothing.

**[`saveFit()`](../reference/saveFit.md) vs `:=`.** When `:=` saves a
fit it calls [`saveFit()`](../reference/saveFit.md) internally; that
inner [`saveFit()`](../reference/saveFit.md) is silent and only the `:=`
reports. So every saved fit is reported exactly once: by `:=` when it
was assigned with `:=`, otherwise by
[`saveFit()`](../reference/saveFit.md).
[`saveFit()`](../reference/saveFit.md) of anything that is not a fit
(for example a plain list) emits nothing.

## What is not emitted

- **Hash evaluations.** To build its cache key, `:=` evaluates the
  arguments of the right-hand side (or, for
  [`nlmixr2()`](https://nlmixr2.github.io/nlmixr2est/reference/nlmixr2.html),
  the estimation properties). That evaluation runs inside an event
  scope, so in `a := augPred(nlmixr2(model, data, est = "posthoc"))` the
  fit computed only to hash the call is silent; only the fit that
  produces the value reports a `fitComplete`.
- **Cache hits produce no work events.** When the value is restored from
  disk, the right-hand side is not run, so the sequence is just
  `assignStart` and `assign` with `cached = TRUE`.
- **Anything inside another operation’s scope.** A `:=` that runs inside
  a fit, a solve or any other scoped operation emits nothing, like every
  other event.

### Nested `:=`

A `:=` evaluated while another `:=` is running its right-hand side (for
example inside a function called on the right) reports normally, and its
`assign` arrives before the outer one:

    assignStart(outer) -> assignStart(inner) -> assign(inner) -> assign(outer)

The outer `:=` saves and restores its own state around the inner one, so
the outer `assign` still carries its own `sha`, and an inner
[`saveFit()`](../reference/saveFit.md) inside either one stays silent.

## Listening

A listener is any `function(event, ...)`. This one just records what it
is told:

``` r

library(nlmixr2save)
library(nlmixr2est)

events <- new.env()
events$log <- list()
rxode2::rxEventListen("demo", function(event, ...) {
  p <- list(...)
  events$log[[length(events$log) + 1L]] <- list(event = event, payload = p)
})
seen <- function() {
  vapply(events$log, function(e) {
    if (is.null(e$payload$name)) e$event else paste0(e$event, "(", e$payload$name, ")")
  }, character(1))
}
```

A small model to fit:

``` r

one <- function() {
  ini({
    tka <- log(1.57); tcl <- log(2.72); tv <- log(31.5)
    eta.ka ~ 0.6; eta.cl ~ 0.3; eta.v ~ 0.1
    add.sd <- 0.7
  })
  model({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    linCmt() ~ add(add.sd)
  })
}
```

The first `:=` runs the fit. When the installed `nlmixr2est` has the
event hooks, its `fitComplete` arrives between the two `nlmixr2save`
events (`assignStart(fit)`, `fitComplete`, `assign(fit)`); with an
`nlmixr2est` without them only the two `nlmixr2save` events are seen:

``` r

fit := nlmixr2(one, nlmixr2data::theo_sd, est = "posthoc",
               control = list(print = 0))
```

``` r

seen()
#> [1] "assignStart(fit)" "fitComplete"      "assign(fit)"
a <- events$log[[length(events$log)]]$payload
a$cached
#> [1] FALSE
a$sha
#> [1] "7627ce8c37bb6ce794f615d561390c0dc1bbc31d"
as.call(as.list(a$call)) # the right-hand side of `:=`
#> nlmixr2(one, nlmixr2data::theo_sd, est = "posthoc", control = list(print = 0))
```

Running it again restores the fit from `fit.zip`; nothing is fit, so
only the assignment is reported:

``` r

events$log <- list()
fit := nlmixr2(one, nlmixr2data::theo_sd, est = "posthoc",
               control = list(print = 0))
seen()
#> [1] "assignStart(fit)" "assign(fit)"
events$log[[2]]$payload$cached
#> [1] TRUE
```

A simulation is flagged as random and carries the seed it started from:

``` r

events$log <- list()
ev <- rxode2::et(amt = 320) |> rxode2::et(0:24)
set.seed(42)
sim := rxode2::rxSolve(fit, ev, nSub = 2)
```

``` r

seen()
#> [1] "assignStart(sim)" "solveComplete"    "assign(sim)"
a <- events$log[[length(events$log)]]$payload
a$random
#> [1] TRUE
!is.null(a$seed)
#> [1] TRUE
a$since
#> [1] 6
```

An error on the right-hand side reports `assignAbort`:

``` r

events$log <- list()
try(bad := nlmixr2(one, nlmixr2data::theo_sd, est = "notAnEst"), silent = TRUE)
seen()
#> [1] "assignStart(bad)" "assignAbort(bad)"
```

A direct [`saveFit()`](../reference/saveFit.md) reports one `assign`
named after the file:

``` r

events$log <- list()
saveFit(fit, "fitCopy")
seen()
#> [1] "assign(fitCopy)"
```

And a nested `:=`:

``` r

events$log <- list()
inner <- function() {
  y := data.frame(b = 2)
  data.frame(c = 3)
}
outer := inner()
seen()
#> [1] "assignStart(outer)" "assignStart(y)"     "assign(y)"         
#> [4] "assign(outer)"
```

Remove the listener when done:

``` r

rxode2::rxEventUnlisten("demo")
```

## Stable fit keys

`:=` caches a result built on a fit (`sim := rxSolve(fit, ev)`,
`a := augPred(fit)`) under a hash of its arguments. Serializing a fit to
hash it is not stable: a fit reloaded from its zip in a new session
differs byte-wise from the fresh one (loader fields, delayed bindings,
tables that went through a csv file). So `:=` hashes a fit on its
right-hand side with
[`nlmixr2saveFitKey()`](../reference/nlmixr2saveFitIdentity.md) instead,
and that result is a cache hit in a new session too. Both keys are lists
meant to be hashed, and neither forces nor writes anything in the fit.

- `nlmixr2saveFitIdentity(fit)` says *which* fit it is: the model
  (normalized model, error lines and initial estimates table), the
  estimation method, the fixed effects, the `omega` matrix and the data.
  It is the same for a fit, its
  [`saveFit()`](../reference/saveFit.md)/[`loadFit()`](../reference/loadFit.md)
  reload, and the fit after
  [`addCwres()`](https://nlmixr2.github.io/nlmixr2est/reference/addCwres.html).
- `nlmixr2saveFitKey(fit)` adds everything a downstream result could
  depend on: the column names, the objective function rows and values,
  the covariance and its method, the table values, and (with
  `sha = TRUE`, the default) the hash `:=` stored on the fit.

Numbers are rounded to 10 to 12 significant digits so a csv round trip
does not change the keys. What invalidates a `:=` cache built on a fit
is anything that changes
[`nlmixr2saveFitKey()`](../reference/nlmixr2saveFitIdentity.md):

| Change to the fit | Identity | Key (invalidates `:=`) |
|----|----|----|
| [`saveFit()`](../reference/saveFit.md) then [`loadFit()`](../reference/loadFit.md), a new R session | same | same |
| [`addCwres()`](https://nlmixr2.github.io/nlmixr2est/reference/addCwres.html), [`addNpde()`](https://nlmixr2.github.io/nlmixr2est/reference/addNpde.html), [`addTable()`](https://nlmixr2.github.io/nlmixr2est/reference/addTable.html) (new columns) | same | changes |
| [`setOfv()`](https://nlmixr2.github.io/nlmixr2est/reference/setOfv.html) (new objective function row) | same | changes |
| a new covariance (`preconditionFit()`, …) | same | changes |
| different estimates, data, model or method | changes | changes |

``` r

fit2 <- loadFit("fitCopy")
identical(digest::digest(nlmixr2saveFitIdentity(fit)),
          digest::digest(nlmixr2saveFitIdentity(fit2)))
#> [1] TRUE
identical(digest::digest(nlmixr2saveFitKey(fit, sha = FALSE)),
          digest::digest(nlmixr2saveFitKey(fit2, sha = FALSE)))
#> [1] TRUE
```

Loggers can use
[`nlmixr2saveFitIdentity()`](../reference/nlmixr2saveFitIdentity.md) to
recognize the same fit across `fitComplete`, `fitUpdate` and `assign`
events, even after it was saved and reloaded.

## Full logging

To record all of these events (with the fits, simulations and results
they describe) to a log, use the
[nlmixr2log](https://github.com/nlmixr2/nlmixr2log) package, which
listens to the bus and turns `assignStart`/`assign` into named entries.
