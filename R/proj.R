#' Reproject sfc columns
#'
#' @param x The object
#' @param sfc_names A character vector of the sfc column names
#' @param crs The projection to use.
#' @return The modified object
#' @export
ps_sfcs_to_crs <- function(x, sfc_names = ps_sfc_names(x),
                           crs = getOption("ps.crs", 4326)) {
  if (!all(sfc_names %in% ps_sfc_names(x)))
    ps_error("missing sfc_names")

  if (!length(sfc_names)) return(x)

  active_sfc_name <- ps_active_sfc_name(x)
  x %<>% ps_deactivate_sfc()

  stop("need to project sfcs to crs")

  if (length(active_sfc_name)) x %<>% ps_activate_sfc(active_sfc_name)
  x
}

#' Reproject sfc columns to WGS84
#'
#' @param x The object
#' @param sfc_names A character vector of the sfc column names
#' @return The modified object
#' @export
ps_sfcs_to_wgs84 <- function(x, sfc_names = ps_sfc_names(x)) {
  ps_sfcs_to_crs(x, sfc_names, crs = 4326)
}

#' Reproject sfcs column to UTMs.
#'
#' The UTM zone is that for the centroid of the centroids of the column.
#'
#' @param x The object
#' @param sfc_names A character vector of the sfc column names
#' @return The modified object
#' @export
ps_sfcs_to_utm <- function(x, sfc_names = ps_sfc_names(x)) {
  x %<>% ps_sfcs_to_wgs84(sfc_names = sfc_names)
  stop("need centroids(s) then proj4 then to project")
#  x %<>% ps_sfcs_to_crs(sfc_names = sfc_names, crs = proj)
  x
}
