#' Name of sfc column(s)
#'
#' @param x The object.
#' @return A character vector of the name of the sfc column(s).
#' @export
ps_sfc_names <- function(x) {
  if (!is.data.frame(x)) return(character(0))

  sfc_names <- colnames(x)
  sfc_names <- sfc_names[vapply(x, is.sfc, TRUE)]
  sfc_names
}

#' Name of active sfc column
#'
#' @param x The object.
#'
#' @return A character vector of the name of the active sfc column.
#' @export
ps_active_sfc_name <- function(x) {
  if (!is.sf(x)) return(character(0))
  if (is.null(attr(x, "sf_column"))) return(character(0))

  attr(x, "sf_column")
}

#' Name of inactive sfc column(s)
#'
#' @param x The object.
#' @return A character vector of the name of the inactive sfc column(s).
#' @export
ps_inactive_sfc_names <- function(x) {
  setdiff(ps_sfc_names(x), ps_active_sfc_name(x))
}

#' Activate sfc column
#'
#' Activates sfc column.
#'
#' @param x The object object.
#' @param sfc_name A string indicating the name of the sfc column to activate.
#' @export
ps_activate_sfc <- function(x, sfc_name = "geometry"){
  check_string(sfc_name)
  if (!sfc_name %in% ps_sfc_names(x)) ps_error("sfc_name must be an sfc column.")

  if (identical(sfc_name, ps_active_sfc_name(x))) return(x)

  if (is.sf(x)) {
    x %<>% sf::st_set_geometry(sfc_name)
  } else {
    x %<>% sf::st_sf(sf_column_name = sfc_name)
  }
  x
}

#' Deactivate active sf column
#'
#' Deactives active geometry column ('sf column').
#'
#' @param x A sf object.
#' @export
#'
ps_deactivate_sfc <- function(x){
  lifecycle::deprecate_soft("0.0.0.9027", "ps_deactivate_sfc()", "tibble::as_tibble()")

  if (identical(ps_active_sfc_name(x), character(0))) return(x)

  x %<>%
    as.data.frame() %>%
    tibble::as_tibble()
  x
}

#' Rename active sfc column
#'
#' Renames the active sfc column.
#'
#' @param x The object.
#' @param new_name A string of the new column name.
#' @return The modified object
#' @export
ps_rename_active_sfc <- function(x, new_name = "geometry") {
  active_sfc_name <- ps_active_sfc_name(x)
  if (!length(active_sfc_name)) ps_error("x does not have an active sfc column")
  check_string(new_name)

  if (identical(new_name, active_sfc_name))
    return(x)

  x %<>% tibble::as_tibble()

  x[new_name] <- x[active_sfc_name]
  x[active_sfc_name] <- NULL

  x %<>% ps_activate_sfc(new_name)
  x
}

#' Remove sfc columns
#'
#' Remove specified sfc (geometry) column(s).
#'
#' @param x An sf object.
#' @param sfc_names A character vector indicating the name of the sfc column(s) to remove.
#' @export
ps_remove_sfcs <- function(x, sfc_names = ps_sfc_names(x)){
  if (!is.data.frame(x)) ps_error("x must be a data.frame")
  check_vector(sfc_names, "")
  if (!any(sfc_names %in% ps_sfc_names(x))) return(x)

  if (ps_active_sfc_name(x) %in% sfc_names) x %<>% tibble::as_tibble()

  sfc_names %<>% intersect(ps_sfc_names(x))
  x <- x[ , setdiff(colnames(x), sfc_names)]
  x
}
