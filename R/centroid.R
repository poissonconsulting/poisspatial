#' Find centroid of POINT sfc
#'
#' @param x The object
#' @param sfc_name A string of the sfc column name
#' @param by A character vector of the columns to get the centroid by.
#' @param nearest A flag indicating whether to return the point closest to the centroid (as opposed to the actual centroid)
#' @return Sf object of centroid
#' @export
ps_sfc_centroid1 <- function(x, sfc_name = ps_active_sfc_name(x), by = character(0),
                             nearest = FALSE){

  check_data(x)
  check_string(sfc_name)
  check_colnames(x, sfc_name)
  check_vector(by, "")
  check_colnames(x, by)
  check_flag(nearest)

  if(sfc_name %in% by) ps_error("sfc_name cannot be in by")

  if(!all(sf::st_is(x[[sfc_name]], "POINT")))
    ps_error("ps_sfcs_centroid1 is only defined for POINT sfc")

  x %<>% ps_activate_sfc(sfc_name)

  crs <- ps_get_proj4string(x)

  if(!length(by)) {
    suppressWarnings(
      c <- sf::st_union(x) %>%
        sf::st_centroid() %>%
        sf::st_sf(geometry = .))
    if(nearest) {
      print("c")
      print(c)
      x <- x[sfc_name]
      print("x")
      print(x)
      c %<>% ps_nearest(x) %>%
        ps_deactivate_sfc()
      print("x2")
      print(c)
      c <- c[paste0(sfc_name, ".y")]
      names(c) <- sfc_name
      c %<>% ps_activate_sfc(sfc_name = sfc_name)
    }
    return(c)
  }

  x %<>%
    plyr::ddply(by, ps_sfc_centroid1, sfc_name = sfc_name, nearest = nearest) %>%
    ps_activate_sfc(sfc_name = sfc_name)

  x
}

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

ps_sfcs_centroid <- function(x, sfc_names = ps_sfc_names(x)){

  check_vector(sfc_names, "", length = c(1L, .Machine$integer.max),
               unique = TRUE)

  x <- x[sfc_names] %>%
    ps_deactivate_sfc()

  if(!ps_equal_crs(x)) ps_error("Sfcs must have same crs.")
  if(any(purrr::map_lgl(x, is_longlat))) ps_warning("Centroids not accurate for long/lat data.")

  crs <- ps_get_proj4string(x[[sfc_names[1]]])

  suppressWarnings(c <- purrr::map(x, function(y){
    y %<>% sf::st_cast("POINT") %>%
      do.call(rbind, .)
  }) %>%
    plyr::ldply() %>%
    dplyr::select(X = 2, Y = 3) %>%
    ps_coords_to_sfc(crs = crs) %>%
    ps_activate_sfc() %>%
    sf::st_union() %>%
    sf::st_centroid())

  sf <- sf::st_sf(geometry = c)
  sf
}


