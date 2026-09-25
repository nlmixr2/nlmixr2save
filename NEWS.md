# nlmixr2save 0.2.1

* `saveFit()` no longer zips and deletes the files of another fit whose name
  extends its own with `-<suffix>` (#10).  It picked a fit's files out of the
  target directory by the name pattern `<file>-*`, so with the loose files of
  `saveFit(fit2, "fit-alt", zip=FALSE)` in the directory, `saveFit(fit)` put
  them in `fit.zip` and then deleted them.  The same match also swept in the
  files an earlier `zip=FALSE` save of the same name left behind, so an item
  the new fit lacks (e.g. `origData` with `data=FALSE`) came back from the old
  fit on load.  `saveFit()` now writes every file into a private temporary
  directory and zips (or, with `zip=FALSE`, copies) only those, so other
  files already in the target directory are never read, zipped or removed.
  A save still replaces the fit saved earlier under the same name: a
  `zip=TRUE` save removes that fit's loose loader `<file>.R`, and a
  `zip=FALSE` save removes its `<file>.zip`, which `loadFit()` would
  otherwise load instead -- each only when it really is a saved fit, so an
  unrelated script or archive of that name is kept.  A target directory that
  is an existing file is now an error rather than being written over.

* A `:=` cache under a `nlmixr2save.prefix` no longer overwrites (and, on
  loading, can no longer lose) an unrelated `<name>.zip` in the cache
  directory while writing or reading `<prefix><name>.zip`, and a prefix that
  names a directory (e.g. `"run1/"`) now creates it instead of silently
  failing to write the cache.

* `loadFit()` now loads a fit given as a path, with or without the `.zip` (or
  `.R`) extension: `loadFit("path/to/fit.zip")` and `loadFit("path/to/fit")`
  both work from any working directory.  It used to append `.zip` to whatever
  it was given, so a path ending in `.zip` looked for `fit.zip.zip` and failed
  with "cannot find fit file" even though the file existed.  It also extracted
  the archive into the working directory rather than beside the `.zip`, so a
  fit in another directory was never found, and a same-named file already in
  the working directory was overwritten and then deleted.  The archive is now
  extracted to a temporary directory instead.

* `loadFit()` also loads a fit that an earlier version saved under a path --
  `saveFit(fit, "models/fit")`, `saveFit(fit, "/home/me/models/fit")` or
  `saveFit(fit, "~/models/fit")` -- from anywhere, including on another
  user's machine.  Those versions wrote the path into the loader script
  inside the archive: it named the fit after the path and read every file
  from it, so loading elsewhere failed with "cannot open file" or
  "Permission denied" for the original location.  A `~` path also garbled
  the names the items were restored under.  The archive is now extracted
  flat, and a loader tied to a path is regenerated from the files in it; the
  restored fit matches one saved without a path.  A `.zip` renamed after
  saving loads too.

* `loadFit(myfit)` with a bare symbol loads `myfit.zip` again when no object
  `myfit` exists; it looked for a file named after the "object not found"
  error instead.

* `saveFit(fit, "path_model/fit")` now writes the files inside `path_model/`
  under the bare name `fit`, creating the directory if needed.  The files, and
  the loader's references to them, used to be named `path_model/fit-...`, so
  the archive stored a `path_model/` folder: unzipping it (including by
  `loadFit()`, which unzipped into the working directory) recreated
  `path_model/` wherever that happened, and the loader only worked from the
  directory it was saved from.

* New vignette, "Keeping fits in a models directory", on sending `:=` caches
  to a directory with `nlmixr2save.dir` (and naming them with
  `nlmixr2save.prefix`), setting that for a whole project, saving and loading
  by path, and committing the directory to version control.

* `loadFit()` no longer rebuilds a fit's models while loading.  A saved fit
  stores its compiled model lists (`foceiModel`, `saemModel`) and its `ui` as
  `rxode2::rxode2()` calls, and all of them were rebuilt on load -- for a
  large model, several long C compilations that looked like a hang, plus a
  parse of the whole model for the `ui`.  Each is now built only when
  something first uses it: estimates, tables, `fixef()`, `summary()` and the
  like need none of them; `print()` and `augPred()` build the `ui`, and
  re-estimation or residual recalculation compiles the model list.
  Re-saving a loaded fit (e.g. `nlmixr2saveShare()`) writes the model lists
  back without compiling them.  Fits saved under a path by an earlier version
  get this too, since their loader is regenerated.

* `loadFit()` (and therefore `:=`) no longer depends on the installed lotri
  to read a cache.  A fit's matrices (`cov`, `omega`, `R`, `phiC`, ...) are
  stored as `lotri({...})` blocks with one row per statement; `loadFit()` now
  reads that form itself and passes anything else to lotri.  Some development
  versions of lotri rejected the named rows `saveFit()` writes ("matrix
  expression should be 'name ~ c(lower-tri)'"), which is what broke the
  `:=` example on the package website, and a cache could not be loaded at all
  without nlmixr2est attached, since the scripts call `lotri()` unqualified.
  Existing caches benefit without being re-saved.

* `loadFit()` now brings a restored `iniDf0` in line with the installed
  rxode2's `iniDf`.  It takes the columns and types from the fit's own `ui`,
  which the installed rxode2 rebuilds on load, rather than from a version
  check.  A cache written before rxode2 added `prior` gains the column (as
  `NA`), and an all-`NA` `prior` -- which reads back from the `.csv` as
  logical -- is character again, as rxode2 keeps it.  The reloaded fit's
  `iniDf0` now matches the original.

# nlmixr2save 0.2.0

* `fit := nlmixr2(...)` no longer fails when data.table is attached after
  nlmixr2save (e.g. `library(nlmixr2); library(data.table)`).  data.table's
  exported `:=` masked nlmixr2save's and errored with "Check that
  is.data.table(DT) == TRUE".  nlmixr2save now re-attaches itself in front
  of data.table when data.table is attached; data.table's `DT[, a := b]` is
  unaffected (#8).

* `saveFit()` now restores every `iniDf0` column with the type it had when
  saved, rather than a fixed list of columns.  rxode2's newer character
  `prior` column is all `NA` for fits without priors, which `read.csv()` read
  back as logical, so a loaded fit's `iniDf0` no longer matched the original.

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
  those came back as `NA`.

* `loadFit()` now repairs a `parHistData$type` that the cache's *own* restore
  script dropped to `NA`.  Those levels are applied by the script stored inside
  the cache, so a cache written before nlmixr2est added a type has no level for
  it and coerces it to `NA` -- and re-saving cannot recover it, because by then
  the string is already gone.  `loadFit()` reads the column back from the
  `-parHistData.csv` in the cache and appends whatever the script was missing,
  so existing caches are repaired in place.

* Saving or loading a fit no longer disturbs another cache whose base name it
  is a suffix of.  The list of files belonging to a fit was matched with an
  unanchored pattern, so a cache named `fit` also matched `myfit-env.R` --
  zipping another cache's files into its own archive and then deleting them
  from disk.  This is reachable whenever unzipped files are lying around, which
  `saveFit(zip=FALSE)` leaves by design.  The names are now matched literally
  rather than as a regexp, so a base name holding a metacharacter (`my.fit` is
  an ordinary R name, and as a pattern its `.` also matched `my_fit`) is
  matched exactly.

* `saveFit(fit, zip=FALSE)` now actually leaves the fit unzipped for a fit table
  (a `nlmixr2FitData`).  The method wrote the fit `.csv` and then called the
  core method with a hardcoded `zip=TRUE`, so the argument was silently ignored
  for every fit that carries data.

# nlmixr2save 0.2.0

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
