# Find centroid of POINT sfc

Uses st_combine as opposed to st_union.

## Usage

``` r
ps_sfc_centroid1(
  x,
  sfc_name = ps_active_sfc_name(x),
  by = character(0),
  nearest = FALSE
)
```

## Arguments

- x:

  The object

- sfc_name:

  A string of the sfc column name

- by:

  A character vector of the columns to get the centroid by.

- nearest:

  A flag indicating whether to return the point closest to the centroid
  (as opposed to the actual centroid)

## Value

Sf object of centroid
