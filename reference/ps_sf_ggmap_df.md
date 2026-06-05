# Ggmap from sf to data.frame

A wrapper function to quickly get plottable ggmap basemap clipped to
padded bbox of sf object.

## Usage

``` r
ps_sf_ggmap_df(x, pad, source, maptype)
```

## Arguments

- x:

  A sf object

- pad:

  A numeric vector indicating amount in metres to pad bbox. If vector of
  length 1, that amount is added to all sides; if vector of length 4
  amount is added to left, top, right and bottom, respectively.

- source:

  Google Maps ("google"), OpenStreetMap ("osm"), Stamen Maps ("stamen"),
  or CloudMade maps ("cloudmade")

- maptype:

  Character string providing map theme. Options available are "terrain",
  "terrain-background", "satellite", "roadmap", and "hybrid" (google
  maps), "terrain", "watercolor", and "toner" (stamen maps), or a
  positive integer for cloudmade maps (see ?get_cloudmademap).

## Value

A data.frame object which can be plotted using: ggplot2::geom_point(data
= x, aes(x = x, y = y, col = rgb(layer.1/255, layer.2/255,
layer.3/255))).
