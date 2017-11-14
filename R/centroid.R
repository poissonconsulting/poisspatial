#' Find centroid of sfcs
#'
#' Centroid can be found for multiple sfc columns and any geometry type.
#' For polygon and linestring data, centroid is calculated from vertices.
#' Sfc columns must have same crs and should not be longitude/latitude.
#'
#' @param x The object
#' @param sfc_names A character vector of the sfc column names
#' @return Sf object of centroid
#' @export

ps_sfcs_centroid <- function(x = sf, sfc_names = ps_sfc_names(x)){

  x <- x[sfc_names] %>%
    ps_deactivate_sfc()

  if(!ps_equal_crs(x)) ps_error("Sfcs must have same crs.")
  if(any(purrr::map_lgl(x, is_longlat))) ps_warning("Centroids not accurate for long/lat data.")

  suppressWarnings(c <- purrr::map(x, function(y){
    y %<>% sf::st_cast("POINT") %>%
      do.call(rbind, .)
  }) %>%
    plyr::ldply() %>%
    dplyr::select(X = 2, Y = 3) %>%
    ps_coords_to_sfc() %>%
    ps_activate_sfc() %>%
    sf::st_union() %>%
    sf::st_centroid())

  sf <- sf::st_sf(geometry = c)
  sf
}


