# Pad bbox

Pad bbox

## Usage

``` r
ps_pad_bbox(x, pad)
```

## Arguments

- x:

  A bbox object (or numeric vector of length 4 indicating xmin, ymin,
  xmax, ymax). Object should not be long/lat.

- pad:

  A numeric vector indicating amount in metres to pad bbox. If vector of
  length 1, amount is added to all sides; if vector of length 4 amount
  is added to left, top, right and bottom, respectively.

## Value

modified object.
