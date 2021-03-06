#' Test if object is vector of length 1 or 4
#'
#' @param x object to test.
#' @return flag.
is_pad <- function(x){
  (length(x) == 1L || length(x) == 4L) & inherits(x, "numeric")
}

#' Test if object is raster
#'
#' @param x object to test.
#' @return flag.
is_raster <- function(x){
  inherits(x, "RasterBrick") || inherits(x, "RasterStack") || inherits(x, "RasterLayer")
}

#' Test if numeric vector is long/lat
#'
#' Tests if longitude is between 180 and -180. Object assumed to be in format c(long, lat, long, lat) as in bbox.
#'
#' @param x object to test.
#' @return flag.
is_longlat_real <- function(x){
  (x[1] < 180 & x[1] > -180)
}


#' Create sfg rectangle
#'
#' @param x A bbox object (or numeric vector of length 4 indicating xmin, ymin, xmax, ymax).
#' @return sfg polygon.
#' @export
ps_sfg_rectangle <- function(x){
  x %<>% as.vector()
  if(length(x) != 4) ps_error("as.vector(x) must have length 4 (xmin, ymin, xmax, ymax")

  mat <- matrix(c(x[1], x[2], x[1], x[4], x[3], x[4], x[3], x[2], x[1], x[2]), ncol=2, byrow=TRUE)
  poly <- st_polygon(list(mat))
  poly
}

#' Create sfc rectangle
#'
#' @inheritParams ps_sfg_rectangle
#' @inheritParams sf::st_sfc
#' @return sfc polygon.
#' @export
ps_sfc_rectangle <- function(x, crs){
  sfg <- ps_sfg_rectangle(x)
  st_sfc(sfg, crs = crs)
}

#' Create sf rectangle
#'
#' @inheritParams ps_sfg_rectangle
#' @param crs coordinate reference system: integer with the EPSG code, or character with proj4string
#' @inheritParams sf::st_sfc
#' @return sfc polygon.
#' @export
ps_sf_rectangle <- function(x, crs){
  sfc <- ps_sfc_rectangle(x, crs = crs)
  st_sf(sfc)
}

#' Pad bbox
#'
#' @param x A bbox object (or numeric vector of length 4 indicating xmin, ymin, xmax, ymax). Object should not be long/lat.
#' @param pad A numeric vector indicating amount in metres to pad bbox.
#' If vector of length 1, amount is added to all sides;
#' if vector of length 4 amount is added to left, top, right and bottom, respectively.
#' @return modified object.
#' @export
ps_pad_bbox <- function(x, pad){

  if(length(as.vector(x)) != 4) ps_error("as.vector(x) must have length 4 (xmin, ymin, xmax, ymax")
  if(!is_pad(pad)) ps_error("pad must be numeric vector with length 1 or 4")

  if(length(pad) == 1L) pad <- rep(pad, 4)
  y <- c(x[1] - pad[1],
         x[2] - pad[2],
         x[3] + pad[3],
         x[4] + pad[4])
  y
}

#' Create bounds
#'
#' Create a sfc polygon rectangle as padded bbox of sf object.
#'
#' @param x A sf object. If long/lat, object is temporarily transformed to UTM to add pad distance.
#' @param pad A numeric vector indicating amount in metres to pad bounds.
#' If vector of length 1, amount is added to all sides;
#' if vector of length 4 amount is added to left, top, right and bottom, respectively.
#' @return sfc polygon.
#' @export
ps_create_bounds <- function(x, pad){

  if(!is.sf(x)) ps_error("x must be a sf object")
  if(!is_pad(pad)) ps_error("pad must be numeric vector with length 1 or 4")

  if(is_longlat(x)) {
    y <- ps_sfcs_to_utm(x)
  } else y <- x

  bbox <- sf::st_bbox(y) %>%
    ps_pad_bbox(pad)

  sfg <- ps_sfg_rectangle(bbox)
  sfc <- st_sfc(sfg, crs = st_crs(y))

  sfc %<>% st_transform(st_crs(x))
  sfc
}

#' Ggmap from bbox
#'
#' Get ggmap basemap from (padded) bbox.
#'
#' @param x A bbox object (or numeric vector of length 4 indicating xmin, ymin, xmax, ymax). Must be long/lat.
#' @param source Google Maps ("google"), OpenStreetMap ("osm"), Stamen Maps ("stamen"), or CloudMade maps ("cloudmade").
#' @param maptype Character string providing map theme. Options available are "terrain", "terrain-background",
#' "satellite", "roadmap", and "hybrid" (google maps), "terrain", "watercolor", and "toner" (stamen maps), or a positive integer for cloudmade maps (see ?get_cloudmademap).
#' @return ggmap object.
#' @export
ps_bbox_ggmap <- function(x, source, maptype){

  x %<>% as.vector()
  if(length(x) != 4) ps_error("as.vector(x) must have length 4 (xmin, ymin, xmax, ymax")
  if(!source %in% c("google", "osm", "stamen", "cloudmade")) ps_error("source must be a valid source readable by ggmap.")
  if(!maptype %in% c("terrain", "terrain-background", "satellite", "roadmap", "hybrid", "watercolor", "toner")) ps_error("maptype must be a valid maptype readable by ggmap.")

  map <- ggmap::get_map(location = x,
                      source = source,
                      maptype = maptype)
  map
}

#' Convert ggmap to raster
#'
#' @param x A ggmap object.
#' @return raster object.
#' @export
ps_ggmap_to_raster <- function(x){
  if(!inherits(x, "ggmap")) ps_error("x must be a ggmap object (e.g. from ps_bbox_ggmap)")

  map_bbox <- attr(x, 'bb')
  .extent <- raster::extent(as.numeric(map_bbox[c(2,4,1,3)]))
  my_map <- raster::raster(.extent, nrow= nrow(x), ncol = ncol(x))
  rgb_cols <- setNames(as.data.frame(t(col2rgb(x))), c('red','green','blue'))
  red <- my_map
  raster::values(red) <- rgb_cols[['red']]
  green <- my_map
  raster::values(green) <- rgb_cols[['green']]
  blue <- my_map
  raster::values(blue) <- rgb_cols[['blue']]
  ras <- raster::stack(red,green,blue)
  ras
}

#' Convert raster to data.frame
#'
#' @param x A raster object.
#' @return A data.frame object which can be plotted using:
#' ggplot2::geom_point(data = x, aes(x = x, y = y, col = rgb(layer.1/255, layer.2/255, layer.3/255))).
#' @export
ps_raster_to_df <- function(x){
  if(!is_raster(x)) ps_error("x must be a raster object (e.g. from ps_ggmap_raster)")

  df <- data.frame(raster::rasterToPoints(x))
  df
}

#' Ggmap from sf
#'
#' Get ggmap basemap from (padded) sf object.
#'
#' @param x A sf object.
#' @param pad A numeric vector indicating amount in metres to pad bbox. If vector of length 1, that amount is added to all sides; if vector of length 4 amount is added to left, top, right and bottom, respectively.
#' @param source Google Maps ("google"), OpenStreetMap ("osm"), Stamen Maps ("stamen"), or CloudMade maps ("cloudmade").
#' @param maptype Character string providing map theme. Options available are "terrain", "terrain-background", "satellite", "roadmap", and "hybrid" (google maps),
#' "terrain", "watercolor", and "toner" (stamen maps),
#' or a positive integer for cloudmade maps (see ?get_cloudmademap).
#' @return ggmap object.
#' @export
ps_sf_ggmap <- function(x, pad, source, maptype){

  if(!is.sf(x)) ps_error("x must be a sf object")
  if(!is_pad(pad)) ps_error("pad must be numeric vector of length 1 or 4")
  if(!source %in% c("google", "osm", "stamen", "cloudmade")) ps_error("source must be a valid source readable by ggmap.")
  if(!maptype %in% c("terrain", "terrain-background", "satellite", "roadmap", "hybrid", "watercolor", "toner")) ps_error("maptype must be a valid maptype readable by ggmap.")

  bbox <- ps_create_bounds(x, pad) %>%
    st_transform(4326) %>%
    st_bbox() %>%
    as.vector()

  map <- ggmap::get_map(location = bbox,
                        source = source,
                        maptype = maptype)

  map
}

#' Ggmap from sf to data.frame
#'
#' A wrapper function to quickly get plottable ggmap basemap clipped to padded bbox of sf object.
#'
#' @param x A sf object
#' @param pad A numeric vector indicating amount in metres to pad bbox. If vector of length 1, that amount is added to all sides; if vector of length 4 amount is added to left, top, right and bottom, respectively.
#' @param source Google Maps ("google"), OpenStreetMap ("osm"), Stamen Maps ("stamen"), or CloudMade maps ("cloudmade")
#' @param maptype Character string providing map theme. Options available are "terrain", "terrain-background", "satellite", "roadmap", and "hybrid" (google maps),
#' "terrain", "watercolor", and "toner" (stamen maps),
#' or a positive integer for cloudmade maps (see ?get_cloudmademap).
#' @return A data.frame object which can be plotted using:
#' ggplot2::geom_point(data = x, aes(x = x, y = y, col = rgb(layer.1/255, layer.2/255, layer.3/255))).
#' @export
ps_sf_ggmap_df <- function(x, pad, source, maptype){

  map <- ps_sf_ggmap(x = x, pad = pad, source = source, maptype = maptype) %>%
    ps_ggmap_to_raster() %>%
    ps_raster_to_df()

  map
}
