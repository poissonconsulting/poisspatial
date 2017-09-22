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
  sfc_names[vapply(x, is.sfc, TRUE)]
  sfc_names %<>% setdiff(ps_sf_name(x))
  sfc_names
}

#' Rename sf column
#'
#' Renames the active geometry column.
#'
#' @param x The object.
#' @param new_name A string of the new column name.
#' @return The modified object
#' @export
ps_rename_sf <- function(x, new_name = "geometry") {
  if (!is.sf(x)) ps_error("x must be an sf object")
  check_string(new_name)

  old_name <- ps_sf_name(x)

  stopifnot(!identical(old_name, character(0)))

  if (identical(new_name, old_name))
    return(x)

  x[new_name] <- x[old_name]
  x[old_name] <- NULL
  sf::st_geometry(x) <- new_name
  x
}

#' Set sf column
#'
#' Set active geometry column ('sf column').
#'
#' This is a wrapper for sf::st_set_geometry().
#'
#' @param x A sf object.
#' @param sfc_column A string indicating the name of the sf column to set.
#' @export
#'
ps_set_sf <- function(x, sfc_column = "geometry"){
  check_string(sfc_column)
  if (identical(sfc_column, ps_sf_name(x))) return(x)

  if (!sfc_column %in% ps_sfc_names(x)) ps_error("sfc_column must be an sfc column.")

  x %<>% sf::st_set_geometry(sfc_column)
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
#' @param sfc_columns A character vector indicating the name of the sfc column(s) to remove.
#' @export
#'
ps_remove_sfcs <- function(x, sfc_columns = ps_sfc_names(x)){
  if (!is.data.frame(x)) ps_error("x must be a data.frame")
  check_vector(sfc_columns, "", min_length = 0L)

  sfc_columns %<>% intersect(ps_sfc_names(x))
  x <- x[ , setdiff(colnames(x), sfc_columns)]
  x
}
