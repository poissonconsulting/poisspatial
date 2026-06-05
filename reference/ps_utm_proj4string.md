# Get UTM proj4string

Get UTM proj4string

## Usage

``` r
ps_utm_proj4string(x, sfc_name = ps_active_sfc_name(x), datum = "WGS84")
```

## Arguments

- x:

  A sf object

- sfc_name:

  A character string indicating name of sfc column with valid crs.

- datum:

  A character string indicating desired datum of UTM proj4string.

## Value

A numeric vector of UTM zone(s).
