# Add Z column to sfc based on google maps

Add Z column to sfc based on google maps

## Usage

``` r
ps_elevation_google(
  x,
  sfc_name = ps_active_sfc_name(x),
  Z = "Z",
  key = Sys.getenv("GOOGLE_MAPS_ELEVATION_API_KEY")
)
```

## Arguments

- x:

  The sf object.

- sfc_name:

  A string of the sfc column name.

- Z:

  A string of the name of the Z coordinate.

- key:

  The string of the google maps elevation api key.

## Value

The sf object with a Z column of the elevation in metres.
