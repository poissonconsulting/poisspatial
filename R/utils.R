#' Is CRS
#'
#' @param x The object to test
#'
#' @return A flag.
#' @export
#' @examples
#' is_crs("blah")
#' is_crs("+init=epsg:3857")
is_crs <- function(x) {
  inherits(try(sf::st_crs(x), silent = TRUE), "crs")
}

#' Is sf Class
#'
#' @param x The object to test.
#'
#' @return A flag.
#' @export
is.sf <- function(x) {
  inherits(x, "sf")
}

#' Is sfc Class
#'
#' @param x The object to test.
#'
#' @return A flag.
#' @export
is.sfc <- function(x) {
  inherits(x, "sfc")
}

#' Get EPSG
#'
#' @param x The object to get the epsg code from
#'
#' @return The epsg code
#' @export
ps_get_epsg <- function(x) {
  sf::st_crs(x)$epsg
}

as_data_frame <- function(x) {
  if (!is.sf(x))
    return(as.data.frame(x))

  x %<>% sf::st_cast(to = "POINT")
  coords <- sf::st_coordinates(x)

  x %<>%
    as.data.frame() %>%
    cbind(coords)
  colnames(x)[duplicated(colnames(x))] %<>% paste0(".2")
  x
}

#' Rename geometry column
#'
#' @param x The object.
#' @param new_name A string of the new column name.
#' @return The modified object
#' @export
ps_rename_sf_column_name <- function(x, new_name = "geometry") {
  old_name <- ps_sf_column_name(x)

  if (!identical(length(old_name), 1L)) ps_error("x must have one geometry column name")

  if (identical(new_name, old_name))
    return(x)

  x[new_name] <- x[old_name]
  x[old_name] <- NULL
  sf::st_geometry(x) <- new_name

  x
}

#' Name of geometry column(s)
#'
#' @param x The object.
#'
#' @return A character vector of the name of the geometry column(s).
#' @export
ps_sf_column_name <- function(x) {
  if (!is.sf(x)) return(character(0))

  colnames <- colnames(x)
  colnames[vapply(x, is.sfc, TRUE)]
}

xml_value <- function(x, name) {
  XML::xmlValue(x[[name]], recursive = FALSE)
}

xml_sapply_value <- function(x, name) {
  XML::xmlSApply(x, xml_value, name)
}

xml_attr <- function(x, attr) {
   XML::xmlGetAttr(x, attr)
}

xml_sapply_attr <- function(x, attr) {
  XML::xmlSApply(x, xml_attr, attr)
}

xml_trkseg_data_frame <- function(x, track) {
  longitude <- xml_sapply_attr(x, "lon") %>%
    as.numeric()
  latitude <- xml_sapply_attr(x, "lat") %>%
    as.numeric()
  elevation <- xml_sapply_value(x, "ele") %>%
    as.numeric()
  datetime <- xml_sapply_value(x, "time") %>%
    strptime("%Y-%m-%dT%H:%M:%SZ", tz = "UTC") %>%
    as.POSIXct()

  tibble::tibble(track, datetime, elevation, latitude, longitude)
}
