# Create bounds

Create a sfc polygon rectangle as padded bbox of sf object.

## Usage

``` r
ps_create_bounds(x, pad)
```

## Arguments

- x:

  A sf object. If long/lat, object is temporarily transformed to UTM to
  add pad distance.

- pad:

  A numeric vector indicating amount in metres to pad bounds. If vector
  of length 1, amount is added to all sides; if vector of length 4
  amount is added to left, top, right and bottom, respectively.

## Value

sfc polygon.
