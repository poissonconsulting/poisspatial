# Nearest Neighbour

Each row in object x is bound with its closest neighbour in object y.
Uses the nabor package.

## Usage

``` r
ps_nearest(x, y, by = c("X", "Y"), dist_col = NULL, ...)
```

## Arguments

- x:

  A data.frame, tibble or sf object.

- y:

  A data.frame, tibble or sf object.

- by:

  A possibly named character vector specifying the column(s) to
  calculate the distance over.

- dist_col:

  A string indicating the name of the column to save the distance in.

- ...:

  Not used

## Details

The column(s) to use when calculating the distances are converted to
numeric values. Missing values are currently not permitted.

sf objects have their sf (active geometry) column renamed to geometry.
The nearest calculation for non-point sf objects is based on feature
vertices.
