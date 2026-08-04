# nlmixr2save 0.2.0

This is a feature release of a package already on CRAN (0.1.0).

This is a resubmission.  The previous 0.2.0 submission was archived by the
incoming pretest with two NOTEs on r-devel-linux-x86_64-debian-gcc; both are
fixed here:

* `Rd files without \usage: 'colon-equals.Rd'` / `\arguments should not be
  documented without \usage.` — the `:=` operator's roxygen block used
  `@usage NULL` while still documenting its arguments.  It now declares
  `@usage x := value`, so `colon-equals.Rd` has a `\usage` section matching the
  function's formals.  (This NOTE is also present on the released 0.1.0.)

* `Running R code in 'testthat.R' had CPU time 3.1 times elapsed time` —
  `tests/testthat.R` now caps the 'rxode2'/OpenMP thread pool when `NOT_CRAN`
  is not `"true"`, following the same policy as 'rxode2' and 'nlmixr2est'.
  `checking tests` now reports a 1.0 CPU/elapsed ratio (`[11s/11s]`, previously
  `[22s/7s]`).

## Changes

The main user-facing additions are the ability to export a fit without the
original subject-level data (`saveFit(data = FALSE)` and the new
`nlmixr2saveShare()`), and recording the `nlmixr2est`/`rxode2` versions a fit
was produced with so a stale cached fit can be detected on load.  See NEWS.md
for the full list.

## R CMD check results

0 errors | 0 warnings | 0 notes, on local Linux (R 4.6.1).

## Released version's check results

The two ERRORs currently shown for 0.1.0 (r-release-windows-x86_64 and
r-oldrel-windows-x86_64) are not from this package: the tests fail because
'rxode2' cannot be loaded on those machines,

    unable to load shared object '.../rxode2/libs/x64/rxode2.dll':
    LoadLibrary failure: Die angegebene Prozedur wurde nicht gefunden.

'rxode2' 5.1.6 has been submitted separately.

## Reverse dependencies

There are no reverse dependencies on CRAN.
