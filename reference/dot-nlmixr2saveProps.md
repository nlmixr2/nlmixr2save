# This gets the properties of the nlmixr2 call for saving and loading purposes

This is used to:

- Determine if this is a simulation method like `rxSolve`

## Usage

``` r
.nlmixr2saveProps(object, data, est = NULL, control = NULL, table = NULL, ...)
```

## Arguments

- object:

  Fitted object or function specifying the model.

- data:

  nlmixr data

- est:

  estimation method (all methods are shown by \`nlmixr2AllEst()\`).
  Methods can be added for other tools

- control:

  The estimation control object. These are expected to be different for
  each type of estimation method

- table:

  The output table control object (like \`tableControl()\`)

- ...:

  additional arguments for nlmixr2 but ignored for this call.

## Value

A list containing

- `object`: the rxode2 model to be estimated

- `data`: the simplified dataset used for estimation

- `est`: the estimation method

- `control`: the control list used for estimation

- `table`: the table used for estimation

- `shaOrig`: the sha1 hash of the original data

- `dataOrig`: the original data

- `sha`: the sha1 hash of the list of properties used for estimation

## Details

- Determine the simplified core dataset that is used for estimation that
  allows caching on the minimum dataset.

## Author

Matthew L. Fidler
