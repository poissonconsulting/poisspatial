# Get UTM zone description.

Get UTM zone description.

## Usage

``` r
ps_utm_note(x, sfc_name = ps_active_sfc_name(x), datum = "WGS84")
```

## Arguments

- x:

  A sf object

- sfc_name:

  A character string indicating name of sfc column with valid crs.

- datum:

  A character string indicating desired datum of UTM proj4string.

## Value

A character vector of UTM zone description(s).
