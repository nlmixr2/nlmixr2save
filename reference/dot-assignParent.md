# This returns or assigns the environment used in the `:=` operator

This returns or assigns the environment used in the `:=` operator

## Usage

``` r
.assignParent(env = NULL)
```

## Arguments

- env:

  environment to assign to; if `NULL` (the default), the current parent
  environment is returned.

## Value

the environment used in the `:=` operator

## Author

Matthew L. Fidler

## Examples

``` r
.assignParent()
#> <environment: 0x564b5cf49f20>
```
