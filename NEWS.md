<!-- NEWS.md is maintained by https://fledge.cynkra.com, contributors should not edit this file -->

# poisspatial 0.2.0 (2024-09-06)
FULL SENTENCES, match to past tense like below. check all these changes and make sure they happened and what they mean.

- Updated RCMD check. 
- Dependencies from source on linux.
- Allows user to retain input columns 
- Remove rgdal


# poisspatial 0.1.0 (2021-11-22)

- Added `ps_nearest_feature()` for joining nearest based on polygon and line sf feature boundaries.
- Added warning for `ps_nearest()` to inform user that joins are based on feature vertices for line and polygon sf objects
- Added `Z = "Z"` argument to `ps_elevation_google()`.
- Added `ps_elevation_google()`.
- Soft deprecate the function `ps_deactivate_sfc()`.
