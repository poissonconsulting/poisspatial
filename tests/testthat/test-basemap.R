context("basemap")

test_that("basemap related functions work", {

  pt <- readRDS(system.file("sf/sf-pt.Rds", package = "poisspatial")) %>%
    ps_sfcs_to_wgs84()

  poly <- readRDS(system.file("sf/sf-poly.Rds", package = "poisspatial")) %>%
    ps_sfcs_to_wgs84()

  bbox <- sf::st_bbox(sf::st_transform(pt, 26911))
  bbox2 <- sf::st_bbox(poly)

  sfg <- ps_sfg_rectangle(bbox)
  expect_is(sfg, "sfg")

  pad <- ps_pad_bbox(bbox, 500)
  expect_is(pad, "numeric")
  expect_identical(length(pad), 4L)
  expect_identical(bbox[[1]]-500, pad[[1]])

  pad2 <- ps_pad_bbox(bbox, c(10, 200, 300, 500))
  expect_error(ps_pad_bbox(bbox, c(10, 20, 30, "a")))
  expect_is(pad, "numeric")
  expect_identical(length(pad), 4L)
  expect_identical(bbox[[2]]-200, pad2[[2]])
  expect_identical(bbox[[4]]+500, pad2[[4]])

  sfc <- ps_create_bounds(poly, 200)
  expect_is(sfc, "sfc")
  expect_true(is_longlat(sfc))
  expect_identical(st_crs(sfc), st_crs(poly))
  expect_identical(length(sfc), 1L)
  expect_identical(length(st_cast(sfc, "POINT")), 5L)

  sfc2 <- ps_create_bounds(st_transform(poly, 26911), c(10, 100, 200, 500))
  expect_error(ps_create_bounds(st_transform(poly, 26911), c(10, 100, "b", 500)))
  expect_is(sfc2, "sfc")
  expect_true(!is_longlat(sfc2))
  expect_identical(st_crs(sfc2), st_crs(st_transform(poly, 26911)))
  expect_identical(length(sfc2), 1L)
  expect_identical(length(st_cast(sfc2, "POINT")), 5L)
  expect_identical(st_bbox(st_transform(poly, 26911))[[1]] -10, st_coordinates(sfc2)[1,1][[1]])
  expect_identical(st_bbox(st_transform(poly, 26911))[[3]] + 200, st_coordinates(sfc2)[3,1][[1]])

  ggmap <- ps_bbox_ggmap(x = bbox2, "google", "satellite")
  expect_is(ggmap, "ggmap")

  ras <- ps_ggmap_to_raster(ggmap)
  expect_true(is_raster(ras))
  expect_error(ps_ggmap_to_raster(poly))

  df <- ps_raster_to_df(ras)
  expect_is(df, "data.frame")
  expect_identical(names(df), c("x", "y", "layer.1", "layer.2", "layer.3"))
  expect_error(ps_raster_to_df(poly))

  ggmap2 <- ps_sf_ggmap(poly, 500, "google", "satellite")
  expect_is(ggmap2, "ggmap")

  ggmap3 <- ps_sf_ggmap_df(poly, 500, "google", "satellite")
  expect_is(ggmap3, "data.frame")
  expect_identical(names(ggmap3), c("x", "y", "layer.1", "layer.2", "layer.3"))
})
