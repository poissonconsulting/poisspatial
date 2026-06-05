# Read a FWA layer.

Internal function that uses shared Poisson dropbox folder.

## Usage

``` r
ps_read_fwa(
  shortcut = NULL,
  gdb = "FWA_BC.gdb",
  layer = "FWA_COASTLINES_SP",
  dir = "~/Poisson/Files - Data/Data/spatial/fwa/gdb/"
)
```

## Arguments

- shortcut:

  A character string indicating shortcut name of layer to read (layer
  stripped of 'FWA\_' and '\_POLY' or '\_SP'). Any value of shortcut
  other than NULL will override layer argument.

- gdb:

  A character string indicating which geodatabase to read. See
  ps_fwa_gdbs() for options.

- layer:

  A character string indicating which layer to read. See ps_fwa_layers()
  for options.

- dir:

  A character string indicating path to directory holding fwa
  geodatabases.

## Value

sf object.
