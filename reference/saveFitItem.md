# Save a fitted model item to a file

This is a generic function to save a fitted model item to a file.

## Usage

``` r
saveFitItem(item, name, file)

# S3 method for class 'rxUi'
saveFitItem(item, name, file)

# S3 method for class 'data.frame'
saveFitItem(item, name, file)

# S3 method for class 'nlmixr2estSessionInfo'
saveFitItem(item, name, file)

# Default S3 method
saveFitItem(item, name, file)

# S3 method for class 'saemFit'
saveFitItem(item, name, file)

# S3 method for class 'foceiModelList'
saveFitItem(item, name, file)
```

## Arguments

- item:

  Item to be saved

- name:

  Name of the item

- file:

  Baseline file name to save the item to.

## Value

boolean to determine if the item was saved; if it wasn't it will be
saved into the general list of items.

## Author

Matthew L. Fidler
