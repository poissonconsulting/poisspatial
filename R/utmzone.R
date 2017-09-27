#' Get UTM Zone
#'
#' Get UTM Zone from Longitude. This basic calculation only works for mid-latitude zones (between -180 and 180 degrees latitude).
#'
#' @param longitude A numeric vector of longitude(s) in decimal degree format.
#' @param datum A character string indicating desired datum ("WGS84", "NAD83").
#' @param hemisphere A character string indicating whether data is in northern or souther hemisphere.
#' @param return A character string indicating format of returned value. ("epsg", "proj4string", "zone number", "zone string").
#'
#' @return
#' @export
ps_utm_zone <- function(longitude = -117, datum = "NAD83", hemisphere = "north", return = "proj4string") {
  check_number(longitude)

  x <- (floor((longitude + 180)/6) %% 60) + 1
  if(hemisphere == "north") {
    prj <-  paste0("+proj=utm +zone=", x, " +datum=", datum, " +units=m +no_defs")
  } else {
    prj <- paste0("+proj=utm +zone=", x, " +south", " +datum=", datum, " +units=m +no_defs")
  }

  if(return == "zone number") {
    y <- x
    y
  }

  if(return == "proj4string") {
    y <- prj
    y
  }

  if(return == "epsg") {
    epsg <- rgdal::make_EPSG()
    y <- dplyr::filter(epsg, prj4 == prj)$code
    y
  }

  if(return == "zone string") {
    epsg <- rgdal::make_EPSG()
    y <- dplyr::filter(epsg, prj4 == prj)$note %>%
      sub("# ", "", .)
    y
  }
}
