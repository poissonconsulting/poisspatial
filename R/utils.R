#' Is CRS
#'
#' @param x The object to test
#'
#' @return A flag.
#' @export
#' @examples
#' is_crs("blah")
#' is_crs("epsg:3857")
#' is_crs(3857)
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

#' Is long/lat
#'
#' @param x The object to test.
#'
#' @return A flag.
#' @export
is_longlat <- function(x) {
  prj <- ps_get_proj4string(x)
  prj == "+proj=longlat +datum=WGS84 +no_defs"
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

punctuate_strings <- function(x, qualifier = "or") {
  if (length(x) == 1)
    return(x)
  n <- length(x)
  paste(paste(x[-n], collapse = ", "), qualifier, x[n])
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

xml_wpt_data_frame <- function(x, wpt) {
  longitude <- xml_attr(x, "lon") %>%
    as.numeric()
  latitude <- xml_attr(x, "lat") %>%
    as.numeric()
  datetime <- xml_value(x, "time") %>%
    strptime("0000-00-00T%H:%M:%SZ", tz = "UTC") %>%
    as.POSIXct()

  tibble::tibble(wpt, datetime, latitude, longitude)
}

warn_geom_non_point <- function(x) {
  if(!all(sf::st_geometry_type(x) %in% c("POINT", "MULTIPOINT"))) ps_warning("Distance calculation uses nearest vertex for non-point geometries. Use `ps_nearest_feature` for calculating nearest feature boundary for lines and polygons.")
}
