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

#' Get EPSG
#'
#' @param x The object to get the epsg code from
#'
#' @return The epsg code
#' @export
ps_get_proj4string <- function(x) {
  sf::st_crs(x)$proj4string
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
