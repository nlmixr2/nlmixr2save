# Load a fitted model object from a file

Load a fitted model object from a file

## Usage

``` r
loadFit(file, checkVersion = .nlmixr2saveCheckVersion())
```

## Arguments

- file:

  the fit to load: the base name of the files it was saved to (`"fit"`
  loads `fit.zip`, or `fit.R` for a fit saved with `zip=FALSE`), or the
  path of that `.zip` or `.R` file itself. It may include a directory,
  e.g. `"path/to/fit"` or `"path/to/fit.zip"`; the working directory is
  not changed and nothing is extracted into it.

- checkVersion:

  when `TRUE`, warn if the fit was produced with a different
  nlmixr2est/rxode2 version (or remote sha) than the one currently
  installed. Defaults to `getOption("nlmixr2save.checkVersion", TRUE)`.

## Value

the fitted model object
