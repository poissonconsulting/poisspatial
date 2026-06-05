# Ggmap from bbox

Get ggmap basemap from (padded) bbox.

## Usage

``` r
ps_bbox_ggmap(x, source, maptype)
```

## Arguments

- x:

  A bbox object (or numeric vector of length 4 indicating xmin, ymin,
  xmax, ymax). Must be long/lat.

- source:

  Google Maps ("google"), OpenStreetMap ("osm"), Stamen Maps ("stamen"),
  or CloudMade maps ("cloudmade").

- maptype:

  Character string providing map theme. Options available are "terrain",
  "terrain-background", "satellite", "roadmap", and "hybrid" (google
  maps), "terrain", "watercolor", and "toner" (stamen maps), or a
  positive integer for cloudmade maps (see ?get_cloudmademap).

## Value

ggmap object.
