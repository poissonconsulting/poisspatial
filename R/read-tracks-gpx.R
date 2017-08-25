#' Read GPX
#'
#' Reads tracks from a gpx file.
#'
#' @param file A string of the path to the file.
#' @param tz A string of the tz for the date times.
#' @param crs An integer of the EPSG for the projection of the points.
#' @return An sf object with a tibble of the datetime and a sfc_POINT geometry of three dimensional points where the third dimension is the elevation in m.
#' @seealso ps_read_tracks_gpxs
#' @export
ps_read_tracks_gpx <- function(file, tz = getOption("ps.tz", "UTC"), crs = getOption("ps.crs", 4326)) {
  check_string(file)

  if (!file.exists(file))
    stop("file '", file, "' does not exist.", call. = FALSE)

  gpx <- XML::xmlTreeParse(file, useInternalNodes = TRUE) %>%
    XML::xmlRoot() %>%
    magrittr::extract("trk")

  if (!length(gpx))
    stop("file '", file, "' does not contain tracks", call. = FALSE)

  names(gpx) <- vapply(gpx, xml_value, character(1), name = "name",
                       USE.NAMES = FALSE)
  gpx %<>%
    lapply(magrittr::extract2, "trkseg") %>%
    purrr::imap(xml_trkseg_data_frame) %>%
    do.call(rbind, .) %>%
    sf::st_as_sf(coords = c("longitude", "latitude", "elevation"), crs = 4326) %>%
    sf::st_transform(crs = crs)

  gpx$datetime %<>% lubridate::with_tz(tz)
  gpx
}

#' Read GPX
#'
#' Reads tracks from a gpx file.
#'
#' @param dir A string of the directory name.
#' @param pattern A string of the pattern to use when searching for files.
#' @param recursive A flag indicating whether to recurse into subdirectories.
#' @inheritParams ps_read_tracks_gpx
#' @return An sf object with a tibble of the datetime (POSIXct) and the file path (character) and a sfc_POINT geometry of three dimensional points where the third dimension is the elevation in m.
#' @seealso ps_read_tracks_gpx
#' @export
ps_read_tracks_gpxs <- function(dir, pattern = "[.]gpx$", recursive = FALSE,
                                tz = getOption("ps.tz", "UTC"),
                                crs = getOption("ps.crs", 4326)) {
  check_string(dir)
  check_string(pattern)
  check_flag(recursive)

  if (!dir.exists(dir))
    stop("directory '", dir, "' does not exist", call. = FALSE)

  files <- list.files(dir, pattern = pattern, recursive = recursive, full.names = TRUE)
  sfiles <- list.files(dir, pattern = pattern, recursive = recursive)

  if (!length(files)) stop("there are no gpx files", call. = FALSE)

  gpx <- lapply(files, ps_read_tracks_gpx, tz = tz, crs = crs) %>%
    stats::setNames(sfiles) %>%
    purrr::imap(function(x, name) {x$file <- name; x}) %>%
    do.call(rbind, .)

  gpx <- gpx[,c("file", "track", "datetime", "geometry")]
  gpx
}
