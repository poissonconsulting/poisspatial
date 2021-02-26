test_that("manipulate geometry column", {
  x <- data.frame(X = c(1,1,10), Y = c(1,10,1))
  x$Row <- 2:4

  x <- ps_coords_to_sfc(x, crs = 28992, activate = FALSE)
  expect_identical(ps_sfc_names(x), "geometry")
  expect_identical(ps_active_sfc_name(x) ,character(0))

  expect_false(is.sf(x))
  x <- ps_activate_sfc(x, "geometry")
  expect_true(is.sf(x))
  expect_identical(ps_inactive_sfc_names(x), character(0))
  expect_identical(ps_active_sfc_name(x), "geometry")

  y <- ps_rename_active_sfc(x, "GEOMETRY")
  expect_identical(ps_active_sfc_name(y), "GEOMETRY")
  y <- ps_activate_sfc(x)
  y <- ps_rename_active_sfc(y)
  expect_equivalent(y, x)
  expect_true(identical(ps_active_sfc_name(y), "geometry"))
  x$geometry.y <- x$geometry
  x$geometry.z <- x$geometry

  z <- ps_sfc_to_coords(x, "geometry.y")
  expect_identical(colnames(z), c("Row", "geometry", "geometry.z", "X", "Y"))
  expect_identical(z$Y, c(1, 10, 1))
  z <- ps_sfc_to_coords(z, "geometry.z", "X1", "Y1")

  z <- ps_deactivate_sfc(z)
  expect_false(is.sf(z))
  expect_identical(colnames(z), c("Row", "geometry", "X", "Y", "X1", "Y1"))

  z <- ps_coords_to_sfc(z, crs = 28992)
  z <- ps_coords_to_sfc(z, c("X1", "Y1"), crs = 28992, sfc_name = "geometry.z")

  expect_identical(colnames(z), c("Row", "geometry", "geometry.z"))

  y <- ps_remove_sfcs(x, "geometry.z")
  expect_identical(colnames(y), c("Row", "geometry", "geometry.y"))

  y <- ps_remove_sfcs(y)
  expect_identical(length(ps_sfc_names(y)), 0L)
  expect_identical(colnames(y), c("Row"))
  expect_true(!is.sf(y))

  ### test when Z column
  x <- data.frame(X = c(1,1,10), Y = c(1,10,1), Z = c(1, 1, 1))
  x$Row <- 2:4
  x <- ps_coords_to_sfc(x, crs = 28992,  coords = c("X", "Y", "Z"), activate = TRUE)
  expect_length(colnames(sf::st_coordinates(x)), 3L)
  y <- ps_sfc_to_coords(x)
  expect_true("Z" %in% names(y))
  expect_identical(y$Z, c("1" = 1, "2" = 1, "3" = 1))

  ### test when linestring
  x <- st_linestring(rbind(c(1, 2), c(2, 3), c(9, 7) )) %>%
    st_sfc(crs = 4326) %>%
    st_sf()
  x$Row <- 2
  y <- ps_sfc_to_coords(x)
  expect_length(colnames(y), 3L)
  expect_identical(y$Row, c(2,2,2))

})
