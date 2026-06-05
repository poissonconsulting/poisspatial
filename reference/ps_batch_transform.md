# Batch transform

Batch transform spatial files. Function finds all files with specified
extension pattern within input directory and sets crs if does not
already exist. Files are projected to new crs and written to output
directory with specified extension and file format (allows any extension
recognised by sf::st_write function).

## Usage

``` r
ps_batch_transform(
  in_dir,
  out_dir,
  recursive = T,
  in_pattern = "[.]shp$",
  out_pattern = ".sqlite",
  in_crs = NULL,
  out_crs = NULL
)
```

## Arguments

- in_dir:

  A string of the path to the directory containing spatial files.

- out_dir:

  A string of the path to the directory that output files will be saved
  to.

- recursive:

  A logical specifying whether to search directory recursively.

- in_pattern:

  A string of the pattern to use when searching for input files.

- out_pattern:

  A string of the extension specifying which file format output files
  will be saved to.

- in_crs:

  An integer of the EPSG that files are currently in (only set if crs
  not currently already present).

- out_crs:

  An integer of the EPSG that files will be transformed to.

## Value

Files with specified crs and extension written to output directory.
