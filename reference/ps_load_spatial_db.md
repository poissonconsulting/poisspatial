# Load spatial database Any spatial database format readable by sf::st_read is acceptable.

Load spatial database Any spatial database format readable by
sf::st_read is acceptable.

## Usage

``` r
ps_load_spatial_db(
  path = "~/Poisson/Files - Data/Data/spatial/fwa/gdb/FWA_BC.gdb",
  layers = NULL,
  crs = NULL,
  rename = identity,
  envir = parent.frame(),
  fun = identity,
  ...
)
```

## Arguments

- path:

  A string of the path to the spatial database.

- layers:

  A character string vector indicating which layers to load.

- crs:

  Default crs of layer if missing.

- rename:

  A function that is used to rename files (after removing extension
  .csv) before they are passed to make.names.

- envir:

  The environment to assign the data frames.

- fun:

  A function that is applied to all sf objects before they are assigned
  to envir.

- ...:

  Additional arguments passed to `st_read`.

## Value

An invisible character vector of the layer names.
