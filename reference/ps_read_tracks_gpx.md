# Read GPX

Reads tracks from a gpx file.

## Usage

``` r
ps_read_tracks_gpx(
  file,
  tz = getOption("ps.tz", "UTC"),
  crs = getOption("ps.crs", 4326)
)
```

## Arguments

- file:

  A string of the path to the file.

- tz:

  A string of the tz for the date times.

- crs:

  An integer of the EPSG for the projection of the points.

## Value

An sf object with a tibble of the datetime and a sfc_POINT geometry of
three dimensional points where the third dimension is the elevation in
m.

## See also

ps_read_tracks_gpxs
