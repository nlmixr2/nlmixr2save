# Load a fitted model object from a file

Load a fitted model object from a file

## Usage

``` r
loadFit(file, checkVersion = .nlmixr2saveCheckVersion())
```

## Arguments

- file:

  the base name of the files to load the fit from.

- checkVersion:

  when `TRUE`, warn if the fit was produced with a different
  nlmixr2est/rxode2 version (or remote sha) than the one currently
  installed. Defaults to `getOption("nlmixr2save.checkVersion", TRUE)`.

## Value

the fitted model object
