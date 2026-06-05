# Is CRS

Is CRS

## Usage

``` r
is_crs(x)
```

## Arguments

- x:

  The object to test

## Value

A flag.

## Examples

``` r
is_crs("blah")
#> [1] FALSE
is_crs("epsg:3857")
#> [1] TRUE
is_crs(3857)
#> [1] TRUE
```
