# Delete every `:=` cache entry under the active prefix and directory

Removes the cache files (`.zip`, `.rds`), the fit loader scripts (`.R`)
and fit component files (`-*.csv`, `-*.rds`, `-*.R`) whose names begin
with the current `getOption("nlmixr2save.prefix")` in the current
`getOption("nlmixr2save.dir")`. Use it to force `:=` to re-run cached
fits/simulations on the next render when `nlmixr2save.check` is `FALSE`.

## Usage

``` r
nlmixr2saveInvalidate()
```

## Value

invisibly, the character vector of files removed

## Author

Matthew L. Fidler

## Examples

``` r
if (FALSE) { # \dontrun{
  options(nlmixr2save.dir = "cache", nlmixr2save.prefix = "modelPiping-")
  nlmixr2saveInvalidate() # clears cache/modelPiping-* entries
} # }
```
