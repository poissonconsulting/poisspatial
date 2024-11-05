#' Find centroid of POINT sfc
#'
#' Uses st_combine as opposed to st_union.
#'
#' @param x The object
#' @param sfc_name A string of the sfc column name
#' @param by A character vector of the columns to get the centroid by.
#' @param nearest A flag indicating whether to return the point closest to the centroid (as opposed to the actual centroid)
#' @return Sf object of centroid
#' @export
ps_sfc_centroid1 <- function(x, sfc_name = ps_active_sfc_name(x), by = character(0),
                             nearest = FALSE) {
  check_data(x)
  chk_string(sfc_name)
  check_names(x, sfc_name)
  chk_vector(by)
  check_values(by, "")
  check_names(x, by)
  chk_flag(nearest)

  if (sfc_name %in% by) ps_error("sfc_name cannot be in by")

  if (!all(sf::st_is(x[[sfc_name]], "POINT"))) {
    ps_error("ps_sfcs_centroid1 is only defined for POINT sfc")
  }

  x %<>% ps_activate_sfc(sfc_name)

  if (!length(by)) {
    suppressWarnings(
      c <- sf::st_combine(x) %>%
        sf::st_centroid() %>%
        sf::st_sf(geometry = .)
    )
    if (nearest) {
      x <- x[sfc_name]
      c %<>% ps_nearest(x) %>%
        tibble::as_tibble()
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
#' @param union A flag indicating whether to use st_union() (the default) versus st_combine()
#' prior to st_centroid()
#' @return Sf object of centroid
#' @export
ps_sfcs_centroid <- function(x, sfc_names = ps_sfc_names(x), union = TRUE) {
  chk_vector(sfc_names)
  check_values(sfc_names, "")
  check_dim(sfc_names, values = TRUE)
  chk_unique(sfc_names)
  chk_flag(union)

  x <- x[sfc_names] %>%
    tibble::as_tibble()

  if (!ps_equal_crs(x)) ps_error("Sfcs must have same crs.")
  if (any(purrr::map_lgl(x, is_longlat))) ps_warning("Centroids not accurate for long/lat data.")

  crs <- sf::st_crs(x[[sfc_names[1]]])

  suppressWarnings(c <- purrr::map(x, function(y) {
    y %<>% sf::st_cast("POINT") %>%
      do.call(rbind, .)
  }) %>%
    plyr::ldply() %>%
    dplyr::select(X = 2, Y = 3) %>%
    ps_coords_to_sfc(crs = crs) %>%
    ps_activate_sfc())

  if (union) {
    c %<>% sf::st_union()
  } else {
    c %<>% sf::st_combine()
  }
  c %<>% sf::st_centroid()

  sf <- sf::st_sf(geometry = c)
  sf
}
