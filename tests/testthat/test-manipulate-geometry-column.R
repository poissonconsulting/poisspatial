context("manipulate-geometry-column")

test_that("manipulate geometry column", {
  x <- data.frame(X = c(1,1,10), Y = c(1,10,1))
  x$Row <- 2:4

  x <- ps_coords_to_sfc(x, crs = 28992)
  expect_identical(ps_sfc_names(x), "geometry")
  expect_identical(ps_sf_name(x), character(0))

  expect_false(is.sf(x))
  x <- ps_set_sf(x, "geometry")
  expect_true(is.sf(x))
  expect_identical(ps_sfc_names(x), character(0))
  expect_identical(ps_sf_name(x), "geometry")

  y <- ps_rename_sf(x, "GEOMETRY")
  expect_identical(ps_sf_name(y), "GEOMETRY")
  y <- ps_rename_sf(y)
  expect_identical(y, x)
  y <- ps_set_sf(x)
  expect_true(identical(ps_sf_name(y), "geometry"))
  x$geometry.y <- x$geometry
  x$geometry.z <- x$geometry

  z <- ps_sfc_to_coords(x, "geometry.y")
  expect_identical(colnames(z), c("Row", "geometry", "geometry.z", "X", "Y"))
  expect_identical(z$Y, c(1, 10, 1))
  z <- ps_sfc_to_coords(z, "geometry.z", "X1", "Y1")

  z <- ps_deactivate_sf(z)
  expect_false(is.sf(z))
  expect_identical(colnames(z), c("Row", "geometry", "X", "Y", "X1", "Y1"))

  z <- ps_coords_to_sf(z, crs = 28992)
  z <- ps_coords_to_sfc(z, c("X1", "Y1"), crs = 28992, sfc_name = "geometry.z")

  expect_identical(colnames(z), c("Row", "geometry", "geometry.z"))

  y <- ps_remove_sfcs(x, "geometry.z")
  expect_true(!"geometry.z" %in% names(y))
  expect_true("geometry.y" %in% names(y))
  y <- ps_remove_sfcs(x)
  expect_identical(length(ps_sfc_names(y)), 0L)
  expect_identical(ps_sf_name(y), "geometry")

  y <- ps_remove_sf(y)
  expect_true(!is.sf(y))
  expect_identical(length(ps_sf_name(y)), 0L)
})

