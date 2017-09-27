#' Name of sf column
#'
#' @param x The object.
#'
#' @return A character vector of the name of the sf (active geometry) column.
#' @export
ps_sf_name <- function(x) {
  if (!is.sf(x)) return(character(0))
  if (is.null(attr(x, "sf_column"))) return(character(0))

  attr(x, "sf_column")
}

#' Name of sfc column(s)
#'
#' @param x The object.
#' @return A character vector of the name of the sfc (inactive geometry) column(s).
#' @export
ps_sfc_names <- function(x) {
  if (!is.data.frame(x)) return(x)

  sfc_names <- colnames(x)
  sfc_names <- sfc_names[vapply(x, is.sfc, TRUE)]
  sfc_names %<>% setdiff(ps_sf_name(x))
  sfc_names
}

#' Rename sf column
#'
#' Renames the active geometry column.
#'
#' @param x The object.
#' @param sf_name A string of the new column name.
#' @return The modified object
#' @export
ps_rename_sf <- function(x, sf_name = "geometry") {
  if (!is.sf(x)) ps_error("x must be an sf object")
  check_string(sf_name)

  old_name <- ps_sf_name(x)

  stopifnot(!identical(old_name, character(0)))

  if (identical(sf_name, old_name))
    return(x)

  x[sf_name] <- x[old_name]
  x[old_name] <- NULL
  sf::st_geometry(x) <- sf_name
  x
}

#' Set sf column
#'
#' Set active geometry column ('sf column').
#'
#' @param x A sf object.
#' @param sfc_name A string indicating the name of the sf column to set.
#' @export
#'
ps_set_sf <- function(x, sfc_name = "geometry"){
  check_string(sfc_name)
  if (identical(sfc_name, ps_sf_name(x))) return(x)

  if (!sfc_name %in% ps_sfc_names(x)) ps_error("sfc_name must be an sfc column.")

  if (is.sf(x)) {
    x %<>% sf::st_set_geometry(sfc_name)
  } else {
    x %<>% sf::st_sf(sf_column_name = sfc_name)
  }
  x
}

#' Deactivate sf column
#'
#' Deactives geometry column ('sf column').
#'
#' @param x A sf object.
#' @export
#'
ps_deactivate_sf <- function(x){
  if (identical(ps_sf_name(x), character(0))) return(x)

  x %<>%
    as.data.frame() %>%
    tibble::as_tibble()
  x
}

#' Remove sf column
#'
#' Remove active geometry column ('sf column').
#'
#' @param x A sf object.
#' @export
#'
ps_remove_sf <- function(x){
  if (identical(ps_sf_name(x), character(0))) return(x)

  sf::st_geometry(x) <- NULL
  x
}

#' Remove sfc columns
#'
#' Remove sfc (inactive geometry) column(s). Active sfc column ('sf column') will not be removed even if specified.
#'
#' @param x An sf object.
#' @param sfc_names A character vector indicating the name of the sfc column(s) to remove.
#' @export
ps_remove_sfcs <- function(x, sfc_names = ps_sfc_names(x)){
  if (!is.data.frame(x)) ps_error("x must be a data.frame")
  check_vector(sfc_names, "", min_length = 0L)

  sfc_names %<>% intersect(ps_sfc_names(x))
  x <- x[ , setdiff(colnames(x), sfc_names)]
  x
}

#' Convert point coordinates to sfc (inactive geometry) column.
#'
#' @param x The object with columns
#' @param coords A character vector of specifying the two columns with the point information.
#' @param crs An integer with the EPSG code, or character with proj4string.
#' @param sfc_name A string of the name of the sfc column.
#' @return The modified object with the coordinates removed
#' @export
ps_coords_to_sfc <- function(x, coords = c("X", "Y"),
                             crs = getOption("ps.crs", 4326),
                             sfc_name = "geometry") {
  if (!is.data.frame(x)) ps_error("x must inherit from a data.frame")
  check_vector(coords, "", min_length = 2L, max_length = 2L)
  check_string(sfc_name)
  check_cols(x, coords)

  sfc <- matrix(c(x[[coords[1]]], x[[coords[2]]]), ncol = 2) %>%
    sf::st_multipoint(dim = "XY") %>%
    sf::st_sfc(crs = crs) %>%
    sf::st_cast("POINT")

  x[coords[1]] <- NULL
  x[coords[2]] <- NULL

  x[[sfc_name]] <- sfc

  x
}

#' Convert point coordinates to sf (active geometry) column.
#'
#' @param x The object with columns
#' @param coords A character vector of specifying the two columns with the point information.
#' @param crs An integer with the EPSG code, or character with proj4string.
#' @param sf_name A string of the name of the sf column.
#' @return The modified object with the coordinates removed
#' @export
ps_coords_to_sf <- function(x, coords = c("X", "Y"),
                            crs = getOption("ps.crs", 4326),
                            sf_name = "geometry") {
  x %<>% ps_coords_to_sfc(coords = coords, crs = crs, sfc_name = sf_name)
  x %<>% ps_set_sf(sf_name)
  x
}

#' Convert sfc (inactive geometry) to pair of coordinates column.
#'
#' @param x The object with columns
#' @param sfc_name A string of the sfc name.
#' @param X A string of the name of the X coordinate.
#' @param Y A string of the name of the Y coordinate.
#' @return The modified object with the sfc column removed
#' @export
ps_sfc_to_coords <- function(x, sfc_name = "geometry", X = "X", Y = "Y") {
  if (!is.data.frame(x)) ps_error("x must inherit from a data.frame")
  check_string(sfc_name)
  check_string(X)
  check_string(Y)

  if (!sfc_name %in% ps_sfc_names(x))
    ps_error("sfc_name '", sfc_name, "' is not an inactive geometry column")

  coords <- sf::st_coordinates(x[[sfc_name]])

  x[[X]] <- coords[,"X",drop = TRUE]
  x[[Y]] <- coords[,"Y",drop = TRUE]
  x[[sfc_name]] <- NULL

  x
}

#' Convert sf (active geometry) to pair of coordinates column.
#'
#' @param x The object with columns
#' @param X A string of the name of the X coordinate.
#' @param Y A string of the name of the Y coordinate.
#' @return A tibble with the sf column removed
#' @export
ps_sf_to_coords <- function(x, X = "X", Y = "Y") {
  if (!is.sf(x)) ps_error("x must be an sf object")

  sfc_name <- ps_sf_name(x)

  x %<>% ps_deactivate_sf()

  ps_sfc_to_coords(x, sfc_name = sfc_name, X = X, Y = Y)
}
