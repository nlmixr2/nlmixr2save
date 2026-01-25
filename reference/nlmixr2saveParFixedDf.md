# Fix nlmixr2 save output

This function modifies the output of nlmixr2's save function to ensure
that the "Estimate" and "SE" fields are numeric vectors with appropriate
names, rather than data frames. This is necessary for compatibility with
other functions that expect these fields to be numeric vectors.

## Usage

``` r
nlmixr2saveParFixedDf(obj)
```

## Arguments

- obj:

  A list object returned by nlmixr2's save function.

## Value

A modified data.frame object with "Estimate" and "SE" as named numeric
vectors
