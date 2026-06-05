# Nearest Feature

Each row in object x is bound with the closest feature in object y. Uses
sf::st_nearest_feature.

## Usage

``` r
ps_nearest_feature(x, y, dist_col = NULL, ...)
```

## Arguments

- x:

  An sf object.

- y:

  An sf object.

- dist_col:

  A string indicating the name of the column to save the distance in.

- ...:

  Not used

## Details

sf objects have their sf (active geometry) column renamed to geometry.
