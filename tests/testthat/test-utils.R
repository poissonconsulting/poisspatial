context("utils")

test_that("utils", {
  x <- data.frame(X = c(1,1,10), Y = c(1,10,1))
  x$Row <- 2:4
  x <- sf::st_as_sf(x, coords = c("X", "Y"), crs = 28992)
  expect_true(is.sf(x))
  expect_true(is.sfc(x$geometry))
  expect_identical(ps_sf_column_name(x), "geometry")

  y <- ps_rename_sf_column_name(x, "GEOMETRY")
  expect_identical(ps_sf_column_name(y), "GEOMETRY")
  y <- ps_rename_sf_column_name(y)
  expect_identical(y, x)

  expect_true(is_crs(28992))
  expect_true(is_crs("+init=epsg:28992"))

  epsg <- ps_get_epsg(x)
  expect_identical(epsg, 28992L)
  expect_warning(ps_get_epsg(1))
})
