# Reproject sfc columns

Reproject sfc columns

## Usage

``` r
ps_sfcs_to_crs(x, sfc_names = ps_sfc_names(x), crs = getOption("ps.crs", 4326))
```

## Arguments

- x:

  The object

- sfc_names:

  A character vector of the sfc column names

- crs:

  The projection to use.

## Value

The modified object
