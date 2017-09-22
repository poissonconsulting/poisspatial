context("utils")

test_that("utils", {
  x <- data.frame(X = c(1,1,10), Y = c(1,10,1))
  x$Row <- 2:4
  x <- sf::st_as_sf(x, coords = c("X", "Y"), crs = 28992)
  expect_true(is.sf(x))
  expect_true(is.sfc(x$geometry))
  expect_identical(ps_sf_name(x), "geometry")

  expect_true(is_crs(28992))
  expect_true(is_crs("+init=epsg:28992"))

  epsg <- ps_get_epsg(x)
  expect_identical(epsg, 28992L)
  proj4string <- ps_get_proj4string(x)
  expect_identical(proj4string, "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.2369,50.0087,465.658,-0.406857,0.350733,-1.87035,4.0812 +units=m +no_defs")
  expect_warning(ps_get_epsg(1))
  expect_true(is_crs(proj4string))
})
