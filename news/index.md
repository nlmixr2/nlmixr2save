# Changelog

## nlmixr2save (development version)

- The `:=` caching operator gains three
  [`options()`](https://rdrr.io/r/base/options.html) (mirroring
  `nlmixr2save.quiet`):

  - `nlmixr2save.prefix` (default `""`): prepended to the assigned
    variable name to form the cache file, e.g. with
    `options(nlmixr2save.prefix = "modelPiping-")`,
    `fit := nlmixr2(...)` caches to `modelPiping-fit.zip`.
  - `nlmixr2save.dir` (default `"."`): the directory the cache files
    live in.
  - `nlmixr2save.check` (default `TRUE`): when `TRUE`, `:=` keeps
    verifying the cache against the current model/data/arguments
    (historical behavior); when `FALSE`, `:=` simply loads the cache
    file if it exists (a fit as `.zip`, a simulation/other value as
    `.rds`) and otherwise runs and saves it – the cache is trusted and
    regenerated only when missing. This keeps a committed cache stable
    across nlmixr2/rxode2 versions.

- New
  [`nlmixr2saveInvalidate()`](../reference/nlmixr2saveInvalidate.md):
  deletes every `:=` cache entry under the active
  `nlmixr2save.prefix`/`nlmixr2save.dir`, so cached fits/simulations are
  re-run on the next render when `nlmixr2save.check` is `FALSE`.

- The saved-fit loader script now quotes its variable name, so a
  `nlmixr2save.prefix` containing non-syntactic characters (e.g. `-`)
  round-trips correctly.

- Support both `$parFixedDf` structures produced by nlmixr2est: the
  current one (named “Estimate”/“SE” columns) and the upcoming
  `$parFixed` refactor (nlmixr2est#645, unnamed columns). The structure
  of the fit is detected when it is saved and recorded in the restore
  script, so fits round-trip exactly with either version
  ([`nlmixr2saveParFixedDf()`](../reference/nlmixr2saveParFixedDf.md)
  gained a `named` argument; existing saved zip files continue to load
  unchanged).

- Additional all-`NA` numeric columns of `$parFixedDf` (for example “CI
  Lower”/“CI Upper” when there is no covariance step) are now coerced
  back to numeric when a saved fit is loaded, instead of being left as
  logical columns from the CSV round-trip.

## nlmixr2save 0.1.0

CRAN release: 2026-06-17

- Create zip files that are (mostly) R independent to save nlmixr2 fit
  items

- Create a cached assignment operator `:=` to save nlmixr2 fits
  automatically; if used, it will load a cached fit if function
  arguments are the same (a sort of disk memoization of the outermost
  function)

- Initial CRAN submission.
