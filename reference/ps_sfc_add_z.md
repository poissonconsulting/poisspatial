# Add elevation (Z) to sf points.

sfc_column and z_column must have identical number of rows for binding.
z_column of 'units' class is accepted. If new_column is NULL, sfc_column
will be replaced with the modifed sfc column.

## Usage

``` r
ps_sfc_add_z(
  x,
  sfc_column = ps_active_sfc_name(x),
  z_column = "Elevation",
  new_column = NULL,
  remove_z = TRUE
)
```

## Arguments

- x:

  A sf object.

- sfc_column:

  A string of the name of the sfc column to add z coordinates to.

- z_column:

  A string of the name of the column containing elevation data.

- new_column:

  A string of the name of the modified sfc column. If NULL, sfc_column
  will be modified.

- remove_z:

  A logical indicating whether to remove the z_column after modifying
  sfc_column.

## Value

The modified object.
