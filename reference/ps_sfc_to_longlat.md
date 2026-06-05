# Convert sfc column to Longitude and Latitude in WGS84.

Convert sfc column to Longitude and Latitude in WGS84.

## Usage

``` r
ps_sfc_to_longlat(x, sfc_name = ps_active_sfc_name(x))
```

## Arguments

- x:

  The sf object.

- sfc_name:

  A string of the sfc column name.

## Value

A tibble with Longitude and Latitude.
