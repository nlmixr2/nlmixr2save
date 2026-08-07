# nlmixr2save 0.2.0

* `loadFit()` (and therefore `:=`) now restores the fit table's `ID` column as a
  factor.  The table round-trips through a plain `.csv`, so `ID` came back as an
  integer while a live fit carries a factor; anything joining the fit table to
  something derived from the fit then hit a type mismatch, since
  `nlme::augPred()` keeps its `id` a factor.  `ggPMX::pmx_nlmixr()` on a cached
  fit failed outright with "Incompatible join types: x.ID (factor) and i.ID
  (integer)".  The repair happens on load, so caches written by earlier versions
  are fixed too.

* `saveFit()` now records the `parHistData$type` factor levels for fits that
  store `parHistData` compressed (the nlmixr2est default).  It read the levels
  straight out of the fit environment, where a compressed fit keeps a raw
  vector rather than a data frame, so the levels were silently not recorded and
  `loadFit()` fell back to a hardcoded level list.  nlmixr2est has since added
  types that list predates ("Analytic Gradient (relaxed)" and friends), and
  those came back as `NA`.  When the levels cannot be recorded at all, the
  restore script now appends any unrecognized type to its hardcoded fallback
  list rather than dropping it to `NA`.  (Unlike the `ID` repair, this lives in
  the restore script inside the zip, so an already-written cache picks it up
  only once it is saved again.)

* `saveFit(fit, zip=FALSE)` now actually leaves the fit unzipped for a fit table
  (a `nlmixr2FitData`).  The method wrote the fit `.csv` and then called the
  core method with a hardcoded `zip=TRUE`, so the argument was silently ignored
  for every fit that carries data.

* `nlmixrDataSimplify()` gained `est` and `control` arguments and no longer
  drops the covariate columns that `est="vae"` searches for.  The VAE covariate
  search picks its covariates out of the data instead of out of the model, so
  the candidate columns (as identified by `nlmixr2est::vaeCovariates()`, matched
  to the `vaeControl()` search options) are now kept in the simplified data and
  included in the hash.  When the installed `nlmixr2est` does not export
  `vaeCovariates()` the search is skipped rather than erroring.

* You can now export a fit **without the original data** so a fitted model can
  be shared when the subject-level data cannot:
    - `saveFit(fit, data = FALSE)` (or `options(nlmixr2save.data = FALSE)`)
      writes a fit whose `origData` is omitted from the zip.
    - New `nlmixr2saveShare()` strips an existing saved fit (a live object or a
      `.zip` base name; honors `nlmixr2save.dir`/`nlmixr2save.prefix`) into a
      shareable sibling zip: `fit-noData.zip` (data removed) or, with
      `noFit = TRUE`, `fit-noData-noFit.zip` (data and the returned
      prediction/residual table removed, keeping the model, estimates, eta table
      and parameter history).  The original fit is left unchanged.

  See `vignette("sharing-fits")`, which also documents the side effects (e.g.
  VPC, residual re-derivation, and re-fitting need the original data).

* `saveFit()` no longer stores the redundant `model` element (the loader always
  rebuilds it from `ui`).  This removes a spurious "could not determine how to
  save object of class call for item model" warning when re-saving a
  previously loaded fit (including via `nlmixr2saveShare()`).

* Saved fits now record the `nlmixr2est` **and** `rxode2` versions they were
  produced with (including each package's commit sha when it was installed from
  a remote such as GitHub).  When a fit is later loaded and the installed
  `nlmixr2est` or `rxode2` differs:
    - `loadFit()` warns that the fit was run with a different package version
      (controllable with the new `checkVersion` argument).
    - the `:=` caching operator, in an interactive session, asks whether to
      rerun the fit with the currently installed packages; non-interactively it
      loads the cached fit and warns.  (Trusted-cache mode,
      `nlmixr2save.check = FALSE`, is left untouched so committed caches stay
      stable across versions.)

  Fits saved by older `nlmixr2save` versions (which carry no version metadata)
  continue to load without any warning.  The whole check can be turned off with
  `options(nlmixr2save.checkVersion = FALSE)` (or per call via
  `loadFit(..., checkVersion = FALSE)`); it is `TRUE` by default.  See
  `vignette("version-tracking")`.

* The `parHistData$type` factor levels are now recorded from the fit at save
  time and restored on load, so the factor round-trips correctly regardless of
  which `nlmixr2est` version produced it (the level set has grown over versions,
  e.g. `"Analytic Gradient"`).  A hardcoded fallback covers fits saved before
  this was recorded.

* The `:=` caching operator gains three `options()` (mirroring
  `nlmixr2save.quiet`):
    - `nlmixr2save.prefix` (default `""`): prepended to the assigned variable
      name to form the cache file, e.g. with `options(nlmixr2save.prefix =
      "modelPiping-")`, `fit := nlmixr2(...)` caches to `modelPiping-fit.zip`.
    - `nlmixr2save.dir` (default `"."`): the directory the cache files live in.
    - `nlmixr2save.check` (default `TRUE`): when `TRUE`, `:=` keeps verifying
      the cache against the current model/data/arguments (historical behavior);
      when `FALSE`, `:=` simply loads the cache file if it exists (a fit as
      `.zip`, a simulation/other value as `.rds`) and otherwise runs and saves
      it -- the cache is trusted and regenerated only when missing.  This keeps
      a committed cache stable across nlmixr2/rxode2 versions.
* New `nlmixr2saveInvalidate()`: deletes every `:=` cache entry under the active
  `nlmixr2save.prefix`/`nlmixr2save.dir`, so cached fits/simulations are re-run
  on the next render when `nlmixr2save.check` is `FALSE`.
* The saved-fit loader script now quotes its variable name, so a
  `nlmixr2save.prefix` containing non-syntactic characters (e.g. `-`) round-trips
  correctly.

* Support both `$parFixedDf` structures produced by nlmixr2est: the
  current one (named "Estimate"/"SE" columns) and the upcoming
  `$parFixed` refactor (nlmixr2est#645, unnamed columns).  The
  structure of the fit is detected when it is saved and recorded in the
  restore script, so fits round-trip exactly with either version
  (`nlmixr2saveParFixedDf()` gained a `named` argument; existing saved
  zip files continue to load unchanged).

* Additional all-`NA` numeric columns of `$parFixedDf` (for example
  "CI Lower"/"CI Upper" when there is no covariance step) are now
  coerced back to numeric when a saved fit is loaded, instead of being
  left as logical columns from the CSV round-trip.

# nlmixr2save 0.1.0

* Create zip files that are (mostly) R independent to save nlmixr2 fit items

* Create a cached assignment operator `:=` to save nlmixr2 fits
  automatically; if used, it will load a cached fit if function
  arguments are the same (a sort of disk memoization of the outermost function)

* Initial CRAN submission.
