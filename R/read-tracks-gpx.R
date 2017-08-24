#' Read GPX
#'
#' Reads tracks from a gpx file.
#'
#' @param file A string of the path to the file.
#' @param tz A string of the tz for the date times.
#' @param crs An integer of the EPSG for the projection of the points.
#' @return An sf object with a tibble of the datetime and a sfc_POINT geometry of three dimensional points where the third dimension is the elevation in m.
#' @export
ps_read_tracks_gpx <- function(file, tz = getOption("ps.tz", "UTC"), crs = getOption("ps.crs", 4326)) {
  check_string(file)

  if (!file.exists(file)) stop("'", file, "' does not exist.", call. = FALSE)

  gpx <- XML::xmlTreeParse(file, useInternalNodes = TRUE) %>%
    XML::xmlRoot() %>%
    XML::xmlToList()

  gpx <- gpx[names(gpx) == "trk"]

  gpx %<>%
    lapply(magrittr::extract2, "trkseg") %>%
    purrr::modify_depth(2, unlist) %>%
    purrr::modify_depth(2, t) %>%
    purrr::modify_depth(2, as.data.frame, stringsAsFactors = FALSE) %>%
    lapply(function(x) do.call(rbind, x)) %>%
    lapply(stats::setNames, c("elevation", "datetime", "latitude", "longitude")) %>%
    do.call(rbind, .)

  gpx$datetime %<>%
    parsedate::parse_iso_8601() %>%
    lubridate::with_tz(tz)

  gpx %<>%
    purrr::map_if(is.character, as.numeric) %>%
    tibble::as_tibble() %>%
    sf::st_as_sf(coords = c("longitude", "latitude", "elevation"), crs = 4326) %>%
    sf::st_transform(crs = crs)

  gpx <- gpx[order(gpx$datetime),]
  gpx
}
