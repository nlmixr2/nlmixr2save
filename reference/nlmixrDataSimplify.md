# Standardize and simplify data for nlmixr2 estimation

This function is typically not needed by end users.

## Usage

``` r
nlmixrDataSimplify(data, object, table = list())
```

## Arguments

- data:

  nlmixr data

- object:

  an nlmixr_ui object (e.g. the output of running
  `nlmixr(object = model)`

- table:

  The output table control object (like \`tableControl()\`)

## Value

The data with the nlmixr2 column lower case and on the left and the
covariate columns on the right and alphabetically sorted.

## Details

The standardization keeps columns that rxode2 and nlmixr2 use along with
the covariates. Column order is standardized (rxode2 then nlmixr2 then
alphabetically sorted covariates), and rxode2 and nlmixr2 column names
are converted to lower case.

## Author

William S. Denney with minor modifications from Matt Fidler
