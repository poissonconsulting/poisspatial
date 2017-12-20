#' Convert point coordinates to sfc column.
#'
#' If sfc_name is the active then the new sfc column replaces it and becomes the
#' active one.
#'
#' @param x The object.
#' @param coords A character vector of specifying the two columns with the point information.
#' @param crs An integer with the EPSG code, or character with proj4string.
#' @param sfc_name A string of the name of the sfc column to create.
#' @return The modified object with the coordinates removed
#' @export
ps_coords_to_sfc <- function(x, coords = c("X", "Y"),
                             crs = getOption("ps.crs", 4326),
                             sfc_name = "geometry") {
  if (!is.data.frame(x)) ps_error("x must inherit from a data.frame")
  check_vector(coords, "", length = 2L)
  check_string(sfc_name)
  check_colnames(x, coords)

  active_sfc_name <- ps_active_sfc_name(x)

  x %<>% ps_deactivate_sfc()

  sfc <- matrix(c(x[[coords[1]]], x[[coords[2]]]), ncol = 2) %>%
    sf::st_multipoint(dim = "XY") %>%
    sf::st_sfc(crs = crs) %>%
    sf::st_cast("POINT")

  x[coords[1]] <- NULL
  x[coords[2]] <- NULL

  x[[sfc_name]] <- sfc

  if (length(active_sfc_name))
    x %<>% ps_activate_sfc(active_sfc_name)
  x
}

#' Convert sfc (geometry) to pair of coordinates column.
#'
#' @param x The object with columns
#' @param sfc_name A string of the sfc name.
#' @param X A string of the name of the X coordinate.
#' @param Y A string of the name of the Y coordinate.
#' @return The modified object with the sfc column removed
#' @export
ps_sfc_to_coords <- function(x, sfc_name = ps_active_sfc_name(x), X = "X", Y = "Y") {
  if (!is.data.frame(x)) ps_error("x must inherit from a data.frame")
  check_string(sfc_name)
  check_string(X)
  check_string(Y)

  if (!sfc_name %in% ps_sfc_names(x))
    ps_error("sfc_name '", sfc_name, "' is not an sfc column")

  if (identical(sfc_name, ps_active_sfc_name(x))) x %<>% ps_deactivate_sfc()

  coords <- sf::st_coordinates(x[[sfc_name]])

  x[[X]] <- coords[,"X",drop = TRUE]
  x[[Y]] <- coords[,"Y",drop = TRUE]
  x[[sfc_name]] <- NULL

  x
}
