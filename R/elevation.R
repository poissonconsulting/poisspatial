#' Add Z column to sfc based on google maps
#'
#' @param x The sf object.
#' @param sfc_name A string of the sfc column name.
#' @param Z A string of the name of the Z coordinate.
#' @param key The string of the google maps elevation api key.
#' @return The sf object with a Z column of the elevation in metres.
#' @export
ps_elevation_google <- function(x, sfc_name = ps_active_sfc_name(x),
                                Z = "Z",
                                key = Sys.getenv("GOOGLE_MAPS_ELEVATION_API_KEY")) {
  y <- ps_sfc_to_longlat(x, sfc_name = sfc_name) # does checking
  chk::chk_string(key)
  chk_string(Z)


  if (!nrow(x)) {
    x[[Z]] <- numeric(0)
    return(x)
  }
  y$..RowID <- seq_len(nrow(y))
  z <- y[!is.na(y$Latitude) & !is.na(y$Longitude), ]
  if (!nrow(z)) {
    x[[Z]] <- NA_real_
    return(x)
  }
  z$lat <- z$Latitude
  z$lon <- z$Longitude
  z$Latitude <- NULL
  z$Longitude <- NULL

  elevation <- googleway::google_elevation(z, key = key)$results$elevation
  if (is.null(elevation)) {
    stop("Invalid key.", call. = FALSE)
  }
  z$Z <- elevation
  z <- z[c("..RowID", "Z")]
  y <- dplyr::left_join(y, z, by = "..RowID")
  y <- y[order(y$..RowID), ]
  x[[Z]] <- y$Z
  x
}
