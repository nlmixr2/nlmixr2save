# nlmixr2save 0.2.0

This is a feature release of a package already on CRAN (0.1.0).

## Changes

The main user-facing additions are the ability to export a fit without the
original subject-level data (`saveFit(data = FALSE)` and the new
`nlmixr2saveShare()`), and recording the `nlmixr2est`/`rxode2` versions a fit
was produced with so a stale cached fit can be detected on load.  See NEWS.md
for the full list.

## Test environments

* local Linux, R release
* GitHub Actions (R-CMD-check): Linux (devel, release, oldrel), macOS, Windows

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies on CRAN.
