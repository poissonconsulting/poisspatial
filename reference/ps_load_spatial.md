# Load spatial files

Load spatial files

## Usage

``` r
ps_load_spatial(
  dir = ".",
  pattern = NULL,
  recursive = FALSE,
  crs = NULL,
  rename = identity,
  envir = parent.frame(),
  fun = identity,
  ...
)
```

## Arguments

- dir:

  A string of the directory.

- pattern:

  An optional regular expression. Only file names which match the
  regular expression will be returned. If NULL, all files with
  extensions readable by st_read wlll be returned.

- recursive:

  A flag indicating whether to include files in subdirectories.

- crs:

  Default crs of object if missing.

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

An invisible character vector of the file names.
