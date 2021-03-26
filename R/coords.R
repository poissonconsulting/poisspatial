#' Convert point coordinates to sfc column.
#'
#' If sfc_name is the active then the new sfc column replaces it and becomes the
#' active one.
#'
#' @param x The object.
#' @param coords A character vector of specifying the two columns with the point information.
#' @param crs An integer with the EPSG code, or character with proj4string.
#' @param sfc_name A string of the name of the sfc column to create.
#' @param activate A flag indicating whether to activate the sfc.
#' @return The modified object with the coordinates removed
#' @export
ps_coords_to_sfc <- function(x, coords = c("X", "Y"),
                             crs = getOption("ps.crs", 4326),
                             sfc_name = "geometry",
                             activate = TRUE) {
  if (!is.data.frame(x)) ps_error("x must inherit from a data.frame")
  check_vector(coords, "", length = 2L:3L)
  check_string(sfc_name)
  check_colnames(x, coords)

  active_sfc_name <- ps_active_sfc_name(x)

  x %<>% tibble::as_tibble()

  x$..ID_coords <- 1:nrow(x)

  y <- x[!is.na(x[[coords[1]]]) & !is.na(x[[coords[2]]]),]

  if(length(coords) == 2L){

    sfc <- matrix(c(y[[coords[1]]], y[[coords[2]]]), ncol = 2) %>%
      sf::st_multipoint(dim = "XY") %>%
      sf::st_sfc(crs = crs) %>%
      sf::st_cast("POINT")
  }

  if(length(coords) == 3L){
      y <- y[!is.na(y[[coords[3]]]),]

      sfc <- matrix(c(y[[coords[1]]], y[[coords[2]]], y[[coords[3]]]), ncol = 3) %>%
      sf::st_multipoint(dim = "XYZ") %>%
      sf::st_sfc(crs = crs) %>%
      sf::st_cast("POINT")
  }

  y[coords[1]] <- NULL
  y[coords[2]] <- NULL
  if(length(coords) == 3L){
    y[coords[3]] <- NULL
  }

  y[[sfc_name]] <- sfc
  colnames <- colnames(y)
  y <- y[c("..ID_coords", sfc_name)]
  x[[sfc_name]] <- NULL

  x <- right_join(y, x, by = "..ID_coords")
  x <- x[colnames]
  x <- x[order(x$..ID_coords),]
  x$..ID_coords <- NULL

  if(activate) {
    x %<>% ps_activate_sfc(sfc_name)
  } else if (length(active_sfc_name))
    x %<>% ps_activate_sfc(active_sfc_name)

  x
}

#' Convert sfc (geometry) to pair of coordinates column.
#'
#' @param x The object with columns
#' @param sfc_name A string of the sfc name.
#' @param X A string of the name of the X coordinate.
#' @param Y A string of the name of the Y coordinate.
#' @param Z A string of the name of the Z coordinate.
#' @return The modified object with the sfc column removed
#' @export
ps_sfc_to_coords <- function(x, sfc_name = ps_active_sfc_name(x), X = "X", Y = "Y", Z = "Z") {
  if (!is.data.frame(x)) ps_error("x must inherit from a data.frame")
  check_string(sfc_name)
  check_string(X)
  check_string(Y)
  check_string(Z)

  if(!(class(x[[sfc_name]])[[1]] %in% c("sfc_LINESTRING", "sfc_MULTILINESTRING", "sfc_POINT", "sfc_MULTIPOINT"))){
    ps_error("sfc_name '", sfc_name, "' must be point or linestring")
  }

  if(class(x[[sfc_name]])[[1]] %in% c("sfc_LINESTRING", "sfc_MULTILINESTRING")){
    x <- st_cast(x, warn = FALSE, "POINT")
  }

  if (!sfc_name %in% ps_sfc_names(x))
    ps_error("sfc_name '", sfc_name, "' is not an sfc column")

  if (identical(sfc_name, ps_active_sfc_name(x))) x %<>% tibble::as_tibble()

  coords <- sf::st_coordinates(x[[sfc_name]])

  x[[X]] <- unname(coords[,"X",drop = TRUE])
  x[[Y]] <- unname(coords[,"Y",drop = TRUE])

  if("Z" %in% colnames(coords)){
    x[[Z]] <- coords[,"Z",drop = TRUE]
  }

  x[[sfc_name]] <- NULL

  x
}
