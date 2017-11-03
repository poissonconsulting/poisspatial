#' Add elevation (Z) to sf points.
#'
#' sfc_column and z_column must have identical number of rows for binding. z_column of 'units' class is accepted.
#' If new_column is NULL, sfc_column will be replaced with the modifed sfc column.
#'
#' @param x A sf object.
#' @param sfc_column A string of the name of the sfc column to add z coordinates to.
#' @param z_column A string of the name of the column containing elevation data.
#' @param new_column A string of the name of the modified sfc column. If NULL, sfc_column will be modified.
#' @param remove_z A logical indicating whether to remove the z_column after modifying sfc_column.
#' @return The modified object.
#' @export
ps_sfc_add_z <- function(x, sfc_column = ps_active_sfc_name(x), z_column = "Elevation", new_column = NULL, remove_z = TRUE){
  if(identical(sfc_column, character(0))) ps_error("There is no active sfc column. Either activate one or specify the name of an inactive sfc column.")
  x %<>% ps_activate_sfc(sfc_column)
  x %<>% cbind(sf::st_coordinates(x))

  sfg <- do.call(list, purrr::map(1:nrow(x), function(y){
    z <- c(x$X[y], x$Y[y], x[[z_column]][y])
    sfg <- sf::st_point(z, dim = "XYZ")
    sfg
  }))

  sfc <- sf::st_sfc(sfg, crs = sf::st_crs(x))

  if(is.null(new_column)){
    x[[sfc_column]] <- sfc
  } else {x[[new_column]] <- sfc}

  if(remove_z){x <- x[, setdiff(names(x), z_column)]}

  x %<>% dplyr::select(-X, -Y)
  x
}

