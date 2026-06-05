# Convert raster to data.frame

Convert raster to data.frame

## Usage

``` r
ps_raster_to_df(x)
```

## Arguments

- x:

  A raster object.

## Value

A data.frame object which can be plotted using: ggplot2::geom_point(data
= x, aes(x = x, y = y, col = rgb(layer.1/255, layer.2/255,
layer.3/255))).
