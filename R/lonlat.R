#' Convert Longitude and Latitude coordinates to sfc (inactive geometry) column.
#'
#' Assumes Longitude and Latitude are in WGS84.
#'
#' @param x The object with columns Latitude and Longitude.
#' @param sfc_name A string of the name of the sf column.
#' @return The modified object with Longitude and Latitude removed
#' @export
ps_lonlat_to_sfc <- function(x, sfc_name = "geometry") {
  ps_coords_to_sfc(x, coords = c("Longitude", "Latitude"), crs = 4326, sfc_name = sfc_name)
}

#' Convert Longitude and Latitude coordinates to sf (active geometry) column.
#'
#' Assumes Longitude and Latitude are in WGS84.
#'
#' @param x The object with columns Latitude and Longitude.
#' @param sf_name A string of the name of the sf column.
#' @return The modified object with Longitude and Latitude removed
#' @export
ps_lonlat_to_sf <- function(x, sf_name = "geometry") {
  ps_coords_to_sf(x, coords = c("Longitude", "Latitude"), crs = 4326, sf_name = sf_name)
}

#' Convert sf (active geometry) column to Longitude and Latitude in WGS84.
#'
#' @param x The sf object.
#' @param sfc_name A string of the sfc column name.
#' @return A tibble with Longitude and Latitude.
#' @export
ps_sfc_to_lonlat <- function(x, sfc_name) {
  x %<>% ps_sfcs_to_wgs84(sfc_names = sfc_name)
  ps_sfc_to_coords(x, sfc_name = sfc_name, X = "Longitude", Y = "Latitude")
}

#' Convert sf (active geometry) column to Longitude and Latitude in WGS84.
#'
#' @param x The sf object.
#' @return A tibble with Longitude and Latitude.
#' @export
ps_sf_to_lonlat <- function(x) {
  x %<>% ps_sf_to_wgs84()
  ps_sf_to_coords(x, X = "Longitude", Y = "Latitude")
}
