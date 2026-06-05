# Convert sfc (geometry) to pair of coordinates column.

Convert sfc (geometry) to pair of coordinates column.

## Usage

``` r
ps_sfc_to_coords(
  x,
  sfc_name = ps_active_sfc_name(x),
  X = "X",
  Y = "Y",
  Z = "Z",
  retain_orig = FALSE
)
```

## Arguments

- x:

  The object with columns

- sfc_name:

  A string of the sfc name.

- X:

  A string of the name of the X coordinate.

- Y:

  A string of the name of the Y coordinate.

- Z:

  A string of the name of the Z coordinate.

- retain_orig:

  A a flag indicating if the input coordinates should be retained as
  columns in the dataframe.

## Value

The modified object with the sfc column removed
