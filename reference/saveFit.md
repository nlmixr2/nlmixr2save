# Save a fitted model object to a series of files

Save a fitted model object to a series of files

## Usage

``` r
saveFit(fit, file, zip = TRUE)

# S3 method for class 'nlmixr2FitCore'
saveFit(fit, file, zip = TRUE)

# S3 method for class 'nlmixr2FitData'
saveFit(fit, file, zip = TRUE)

# Default S3 method
saveFit(fit, file, zip = TRUE)
```

## Arguments

- fit:

  the fitted model object

- file:

  the base name of the files to save the fit to.

- zip:

  Boolean indicating if the files should be zipped.

## Value

nothing, called for side effects

## Author

Matthew L. Fidler
