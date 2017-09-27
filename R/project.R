#' #' Reproject sfc (inactive geometry) columns
#' #'
#' #' @param x The object
#' #' @param sfc_names A character vector of the sfc column names
#' #' @param crs The projection to use.
#' #' @return The modified object
#' #' @export
#' ps_sfcs_to_crs <- function(x, sfc_names = ps_sfc_names(x),
#'                            crs = getOption("ps.crs", 4326)) {
#'   stop("need to project sfcs to crs")
#'   x
#' }
#'
#' #' Reproject sf (active geometry) column
#' #'
#' #' @param x The object
#' #' @param crs The projection to use.
#' #' @return The modified object
#' #' @export
#' ps_sf_to_crs <- function(x, crs) {
#'   if (!is.sf(x)) ps_error("x must be an sf object")
#'
#'   sf_name <- ps_sf_name(x)
#'   x %<>% ps_deactivate_sf() %>%
#'     ps_sfcs_to_crs(sfc_names = sf_name, crs = crs) %>%
#'     ps_set_sf(sf_name)
#'   x
#' }
#'
#' #' Reproject sfc (inactive geometry) columns to WGS84
#' #'
#' #' @param x The object
#' #' @return The modified object
#' #' @export
#' ps_sfcs_to_wgs84 <- function(x, sfc_names = ps_sfc_names(x)) {
#'   ps_sfcs_to_crs(x, sfc_names, crs = 4326)
#' }
#'
#' #' Reproject sf (active geometry) column to WGS84
#' #'
#' #' @param x The object
#' #' @return The modified object
#' #' @export
#' ps_sf_to_wgs84 <- function(x) {
#'   ps_sf_to_crs(x, crs = 4326)
#' }
#'
#' #' Reproject sf (active geometry) column to UTMs.
#' #'
#' #' The UTM zone is that for the centroid of the column.
#' #'
#' #' @param x The object
#' #' @return The modified object
#' #' @export
#' ps_sfcs_to_utm <- function(x, sfc_names = ps_sfc_names(x)) {
#'   x %<>% ps_sfcs_to_wgs84(sfc_names = sfc_names)
#'   stop("need centroids(s) then proj4 then to project")
#' }
#'
#'
#' #' Reproject sf (active geometry) column to UTM.
#' #'
#' #' The UTM zone is that for the centroid of the column.
#' #'
#' #' @param x The object
#' #' @return The modified object
#' #' @export
#' ps_sf_to_utm <- function(x) {
#'   if (!is.sf(x)) ps_error("x must be an sf object")
#'
#'   sf_name <- ps_sf_name(x)
#'   x %<>% ps_deactivate_sf() %>%
#'     ps_sfcs_to_utm(sfc_names = sf_name) %>%
#'     ps_set_sf(sf_name)
#'   x
#' }
#'
#' #' Reproject all sf (active geometry) and sfc (inactive) columns to UTM zone.
#' #'
#' #' The UTM zone is that for the centroid of the centroid of the columns.
#' #'
#' #' @param x The object
#' #' @return The modified object
#' #' @export
#' ps_sf_sfcs_to_utm <- function(x, sf_sfc_names = c(sf_name(x), sfc_names(x))) {
#'   if (!is.sf(x)) ps_error("x must be an sf object")
#'
#'   sf_name <- ps_sf_name(x)
#'   x %<>% ps_deactivate_sf() %>%
#'     ps_sfcs_to_utm(sfc_names = sf_sfc_names) %>%
#'     ps_set_sf(sf_name)
#'   x
#' }
