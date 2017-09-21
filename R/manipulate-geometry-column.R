#' Name of sfc column(s)
#'
#' @param x The object.
#'
#' @return A character vector of the name of the geometry column(s) ('sfc column(s)).
#' @export
ps_sfc_name <- function(x) {
  if (!is.sf(x)) return(character(0))

  colnames <- colnames(x)
  colnames[vapply(x, is.sfc, TRUE)]
}

#' Name of sf column
#'
#' @param x The object.
#'
#' @return A character vector of the name of the active geometry column ('sf column').
#' @export
ps_sf_name <- function(x) {
  if (!is.sf(x)) return(character(0))
  if(is.null(attr(x, "sf_column"))) return(character(0))

  attr(x, "sf_column")
}

#' Rename sf column
#'
#' @param x The object.
#' @param new_name A string of the new column name.
#' @return The modified object
#' @export
ps_rename_sf <- function(x, new_name = "geometry") {

  old_name <- ps_sf_name(x)

  if (!identical(length(old_name), 1L)) ps_error("x must have a sf column")
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
#' @param sf_column A string indicating the name of sf column to set.
#' @export
#'
ps_set_sf <- function(x, sf_column = "geometry"){
  if(!sf_column %in% ps_sfc_name(x)) return("sf_column must be a sfc column.")

  x %<>% sf::st_set_geometry(sf_column)
}

#' Remove sfc column
#'
#' Remove sfc column(s). Active sfc column ('sf column') will not be removed even if specified.
#'
#' @param x A sf object.
#' @param sfc_column A string indicating the name of the sfc column(s) to remove.
#' @export
#'
ps_remove_sfc <- function(x, sfc_column){

  if(!sfc_column %in% ps_sfc_name(x)) return("sfc_column must be sfc column(s).")

  x <- x[ , setdiff(names(x), sfc_column)]
}

#' Remove any sfc column
#'
#' All sfc columns are removed. Active sfc column ('sf column') is not removed.
#'
#' @param x A sf object.
#' @export
#'
ps_remove_any_sfc <- function(x){
  sfc_names <- ps_sfc_name(x)

  if(identical(length(sfc_names), 0L)) return("there are no sfc columns present.")

   x <- x[ , setdiff(names(x), sfc_names)]
}

#' Remove sf column
#'
#' Remove active geometry column ('sf column').
#'
#' @param x A sf object.
#' @export
#'
ps_remove_sf <- function(x){
  if(identical(length(ps_sf_name(x)), 0L)) return("There is no sf column present.")

  sf::st_geometry(x) <- NULL
}
