# Convert point coordinates to sfc column.

If sfc_name is the active then the new sfc column replaces it and
becomes the active one.

## Usage

``` r
ps_coords_to_sfc(
  x,
  coords = c("X", "Y"),
  crs = getOption("ps.crs", 4326),
  sfc_name = "geometry",
  activate = TRUE,
  retain_orig = FALSE
)
```

## Arguments

- x:

  The object.

- coords:

  A character vector of specifying the two columns with the point
  information.

- crs:

  An integer with the EPSG code, or character with proj4string.

- sfc_name:

  A string of the name of the sfc column to create.

- activate:

  A flag indicating whether to activate the sfc.

- retain_orig:

  A flag indicating if the input coordinates should be retained as
  columns in the dataframe.

## Value

The modified object with the coordinates removed
