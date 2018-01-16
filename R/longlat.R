#' Convert Longitude and Latitude coordinates to sfc column.
#'
#' Assumes Longitude and Latitude are in WGS84.
#'
#' @inheritParams ps_coords_to_sfc
#' @param x The object with columns Latitude and Longitude.
#' @return The modified object with Longitude and Latitude removed
#' @export
ps_longlat_to_sfc <- function(x, sfc_name = "geometry", activate = TRUE) {
  ps_coords_to_sfc(x, coords = c("Longitude", "Latitude"), crs = 4326, sfc_name = sfc_name, activate = activate)
}

#' Convert sfc column to Longitude and Latitude in WGS84.
#'
#' @param x The sf object.
#' @param sfc_name A string of the sfc column name.
#' @return A tibble with Longitude and Latitude.
#' @export
ps_sfc_to_longlat <- function(x, sfc_name = ps_active_sfc_name(x)) {
  x %<>% ps_sfcs_to_wgs84(sfc_names = sfc_name)
  ps_sfc_to_coords(x, sfc_name = sfc_name, X = "Longitude", Y = "Latitude")
}
