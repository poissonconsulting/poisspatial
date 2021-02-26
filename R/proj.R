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

  x <- purrr::modify_at(x, .at = sfc_names, .f = sf::st_transform, crs)

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

#' Check that sfcs have same crs
#'
#' @param x The object
#' @param sfc_names A character vector of the sfc column names
#' @return Logical
ps_equal_crs <- function(x, sfc_names = ps_sfc_names(x)) {
  x %<>% ps_deactivate_sfc()
  x <- x[sfc_names]
  crs <- purrr::map2(x, sfc_names, function(y, z){
    c <- ps_get_proj4string(y[z])
  }) %>% unlist()

  !length(unique(crs)) > 1
}

#' Get UTM zone
#'
#' @param x A sf object
#' @param sfc_name A character string indicating name of sfc column.
#' @return A numeric vector of UTM zone(s).
ps_utm_zone <- function(x, sfc_name = ps_active_sfc_name(x)) {
  check_string(sfc_name)

  if (!sfc_name %in% ps_sfc_names(x)) ps_error("column '", sfc_name,"' is not an sfc column")

  x %<>% ps_sfcs_to_wgs84(sfc_names = sfc_name)

  coords <- ps_sfc_to_coords(x, sfc_name = sfc_name)

  long <- coords$X
  lat <- coords$Y

  get_zone <- function(lat, long){
    if(lat >= 56 && lat < 64 && long >= 3 && long < 12){x <- 32} else if(
      lat >= 72 && lat < 84 && long >= 0 && long < 9) {x <- 31} else if(
        lat >= 72 && lat < 84 && long >= 9 && long < 21) {x <- 33} else if(
          lat >= 72 && lat < 84 && long >= 21 && long < 33) {x <- 35} else if(
            lat >= 72 && lat < 84 && long >= 33 && long < 42) {x <- 37} else{
              x <- (floor((long + 180)/6) %% 60) + 1
            }
  }

  purrr::map2_dbl(lat, long, get_zone)
}

#' Get UTM proj4string
#'
#' @param x A sf object
#' @param sfc_name A character string indicating name of sfc column with valid crs.
#' @param datum A character string indicating desired datum of UTM proj4string.
#'
#' @return A numeric vector of UTM zone(s).
ps_utm_proj4string <- function(x, sfc_name = ps_active_sfc_name(x), datum = "WGS84") {
  zone <- ps_utm_zone(x, sfc_name)
  lat <- ps_sfc_to_coords(x[sfc_name])$Y

  prj <- purrr::map2_chr(zone, lat, function(y, z){
    if (z >= 0){
      paste0("+proj=utm +zone=", y, " +datum=", datum, " +units=m +no_defs +type=crs")
    } else{
      paste0("+proj=utm +zone=", y, " +south", " +datum=", datum, " +units=m +no_defs +type=crs")
    }})
  prj
}

#' Get UTM zone description.
#'
#' @param x A sf object
#' @param sfc_name A character string indicating name of sfc column with valid crs.
#' @param datum A character string indicating desired datum of UTM proj4string.
#'
#' @return A character vector of UTM zone description(s).
ps_utm_note <- function(x, sfc_name = ps_active_sfc_name(x), datum = "WGS84") {

  prj <- ps_utm_proj4string(x)

  epsg <- rgdal::make_EPSG()
  chr <-  purrr::map_chr(prj, function(y){
    note <- epsg[epsg$prj4 == y, "note"]
    note <- note[!is.na(note)] %>%
      sub("# ", "",.)
  })
  chr
}

#' Reproject sfc columns to UTMs.
#'
#' The UTM zone is that for the centroid of the centroids of the columns.
#'
#' @param x The object
#' @param sfc_names A character vector of the sfc column names
#' @return The modified object
#' @export
ps_sfcs_to_utm <- function(x, sfc_names = ps_sfc_names(x)) {
  x %<>% ps_sfcs_to_wgs84(sfc_names = sfc_names)

  c <- suppressWarnings(ps_sfcs_centroid(x, sfc_names = sfc_names))

  prj <- ps_utm_proj4string(c)

  x %<>% ps_sfcs_to_crs(sfc_names = sfc_names, crs = prj)

  note <- ps_utm_note(c)

  message(paste(note, "was used to transform sfc column(s)."))
  x
}
