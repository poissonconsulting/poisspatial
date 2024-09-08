<!-- NEWS.md is maintained by https://fledge.cynkra.com, contributors should not edit this file -->

# poisspatial 0.2.0 (2024-09-06)
FULL SENTENCES, match to past tense like below. check all these changes and make sure they happened and what they mean.

- RCMD checks were updated. 
- Dependencies are now loaded from the source files on linux.
- The dependcey of `rgdal` was removed by storing a hard copy of epsg descriptions instead of retrieving them from `rgdal::make_EPSG()`.
- Added `retain_orig` boolean flag to `ps_coords_to_sfc()` and `ps_sfc_to_coords()` allowing user to retain input columns if desired. 

# poisspatial 0.1.0 (2021-11-22)

- Added `ps_nearest_feature()` for joining nearest based on polygon and line sf feature boundaries.
- Added warning for `ps_nearest()` to inform user that joins are based on feature vertices for line and polygon sf objects
- Added `Z = "Z"` argument to `ps_elevation_google()`.
- Added `ps_elevation_google()`.
- Soft deprecate the function `ps_deactivate_sfc()`.
