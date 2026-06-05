# Read GPX

Reads tracks from a gpx file.

## Usage

``` r
ps_read_tracks_gpxs(
  dir,
  pattern = "[.]gpx$",
  recursive = FALSE,
  tz = getOption("ps.tz", "UTC"),
  crs = getOption("ps.crs", 4326)
)
```

## Arguments

- dir:

  A string of the directory name.

- pattern:

  A string of the pattern to use when searching for files.

- recursive:

  A flag indicating whether to recurse into subdirectories.

- tz:

  A string of the tz for the date times.

- crs:

  An integer of the EPSG for the projection of the points.

## Value

An sf object with a tibble of the datetime (POSIXct) and the file path
(character) and a sfc_POINT geometry of three dimensional points where
the third dimension is the elevation in m.

## See also

ps_read_tracks_gpx
