ps_sf_to_wgs84 <- function(x) {
  if (!is.sf(x)) ps_error("x must be an sf object")
  stop("need to project")
}

ps_sfcs_to_wgs84 <- function(x, sfc_columns = ps_sfc_names(x)) {
   stop("need to project")
}

#
# ps_sf_to_utm <- function(x) {
#   x %<>% ps_sf_to_wgs84()
#   sf <- ps_sf_name(x)
#
#   stop("need to project")
# }
#
# ps_sfcs_to_utm <- function(x, sfc_columns = ps_sfc_names(x)) {
#   x %<>% ps_sfcs_to_wgs84()
#   stop("need to project")
# }
