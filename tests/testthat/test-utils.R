test_that("utils", {
  x <- data.frame(X = c(1, 1, 10), Y = c(1, 10, 1))
  x$Row <- 2:4
  x <- sf::st_as_sf(x, coords = c("X", "Y"), crs = 28992)
  expect_true(is.sf(x))
  expect_true(is.sfc(x$geometry))
  expect_identical(ps_active_sfc_name(x), "geometry")

  expect_true(is_crs(28992))
  expect_true(is_crs("epsg:28992"))

  epsg <- ps_get_epsg(x)
  expect_identical(epsg, 28992L)
  proj4string <- ps_get_proj4string(x)
  expect_true(is_crs(proj4string))
  expect_warning(ps_get_epsg(1))
  expect_true(is_crs(proj4string))
})
