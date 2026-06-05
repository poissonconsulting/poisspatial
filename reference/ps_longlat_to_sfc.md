# Convert Longitude and Latitude coordinates to sfc column.

Assumes Longitude and Latitude are in WGS84.

## Usage

``` r
ps_longlat_to_sfc(x, sfc_name = "geometry", activate = TRUE)
```

## Arguments

- x:

  The object with columns Latitude and Longitude.

- sfc_name:

  A string of the name of the sfc column to create.

- activate:

  A flag indicating whether to activate the sfc.

## Value

The modified object with Longitude and Latitude removed
