# Fix nlmixr2 save output

This function modifies the output of nlmixr2's save function to ensure
that the numeric columns of the `parFixedDf` data frame have the types
that nlmixr2 outputs after a CSV round-trip (all-`NA` columns are read
back as logical vectors, so they are coerced back to numeric here).

## Usage

``` r
nlmixr2saveParFixedDf(obj, named = TRUE)
```

## Arguments

- obj:

  A list object returned by nlmixr2's save function.

- named:

  Logical; when `TRUE` (default, matching fits from older nlmixr2est)
  the "Estimate" and "SE" columns are named using the row names; when
  `FALSE` they are left as unnamed numeric vectors.

## Value

A modified data.frame object with numeric columns restored to the
structure of the original fit

## Details

Depending on the version of nlmixr2est that created the fit, the
"Estimate" and "SE" columns are either named numeric vectors (names
matching the row names; nlmixr2est \<= 6.0) or plain unnamed numeric
vectors (the `$parFixed` refactor in newer nlmixr2est). The `named`
argument selects which structure is restored;
[`nlmixr2save::saveFit()`](saveFit.md) records the correct value in the
generated restore script based on the fit being saved.
