# Manage functions treated as random by `:=`

Functions registered here are saved with random-state metadata so cached
restores behave like the original call was run again.

## Usage

``` r
saveFitRandom(fun = NULL, remove = FALSE)
```

## Arguments

- fun:

  Function name(s) to add or remove. If `NULL`, the current registry is
  returned without modification.

- remove:

  Boolean indicating if `fun` should be removed from the registry
  instead of added.

## Value

Character vector of registered function names.

## Author

Matthew L. Fidler

## Examples

``` r
saveFitRandom()
#> [1] "rxSolve"  "simulate" "sim"      "mrgsim"   "predict"  "vpcSim"  
saveFitRandom("myRandomFun")
#> [1] "rxSolve"     "simulate"    "sim"         "mrgsim"      "predict"    
#> [6] "vpcSim"      "myRandomFun"
saveFitRandom("myRandomFun", remove=TRUE)
#> [1] "rxSolve"  "simulate" "sim"      "mrgsim"   "predict"  "vpcSim"  
```
