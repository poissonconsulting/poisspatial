# Changelog

## poisspatial 0.2.1.9000 (2026-06-18)

- Add fledge-bump workflow
- Add fledge-tag-on-merge workflow

## poisspatial 0.2.1 (2025-09-11)

- Update default directories for
  [`ps_load_spatial_db()`](https://poissonconsulting.github.io/poisspatial/reference/ps_load_spatial_db.md),
  [`ps_fwa_gdbs()`](https://poissonconsulting.github.io/poisspatial/reference/ps_fwa_gdbs.md),
  [`ps_fwa_layers()`](https://poissonconsulting.github.io/poisspatial/reference/ps_fwa_layers.md),
  [`ps_fwa_shortcuts()`](https://poissonconsulting.github.io/poisspatial/reference/ps_fwa_shortcuts.md),
  and
  [`ps_read_fwa()`](https://poissonconsulting.github.io/poisspatial/reference/ps_read_fwa.md).

## poisspatial 0.2.0 (2024-09-06)

- The RCMD checks were updated.
- Dependencies are now loaded from the source files on linux.
- The dependency of `rgdal` was removed by storing a hard copy of epsg
  descriptions instead of retrieving them from `rgdal::make_EPSG()`.
- Added `retain_orig` boolean flag to
  [`ps_coords_to_sfc()`](https://poissonconsulting.github.io/poisspatial/reference/ps_coords_to_sfc.md)
  and
  [`ps_sfc_to_coords()`](https://poissonconsulting.github.io/poisspatial/reference/ps_sfc_to_coords.md)
  allowing user to retain input columns if desired.

## poisspatial 0.1.0 (2021-11-22)

- Added
  [`ps_nearest_feature()`](https://poissonconsulting.github.io/poisspatial/reference/ps_nearest_feature.md)
  for joining nearest based on polygon and line sf feature boundaries.
- Added warning for
  [`ps_nearest()`](https://poissonconsulting.github.io/poisspatial/reference/ps_nearest.md)
  to inform user that joins are based on feature vertices for line and
  polygon sf objects
- Added `Z = "Z"` argument to the function
  [`ps_elevation_google()`](https://poissonconsulting.github.io/poisspatial/reference/ps_elevation_google.md).
- Added
  [`ps_elevation_google()`](https://poissonconsulting.github.io/poisspatial/reference/ps_elevation_google.md).
- The function
  [`ps_deactivate_sfc()`](https://poissonconsulting.github.io/poisspatial/reference/ps_deactivate_sfc.md)
  was soft deprecated.
