# Changelog

## nlmixr2save (development version)

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
