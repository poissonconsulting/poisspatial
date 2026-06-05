# Find centroid of sfcs

Centroid can be found for multiple sfc columns and any geometry type.
For polygon and linestring data, centroid is calculated from vertices.
Sfc columns must have same crs and should not be longitude/latitude.

## Usage

``` r
ps_sfcs_centroid(x, sfc_names = ps_sfc_names(x), union = TRUE)
```

## Arguments

- x:

  The object

- sfc_names:

  A character vector of the sfc column names

- union:

  A flag indicating whether to use st_union() (the default) versus
  st_combine() prior to st_centroid()

## Value

Sf object of centroid
