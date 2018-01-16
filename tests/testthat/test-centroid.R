context("centroid")

test_that("works", {

  pt <- readRDS(system.file("sf/pt.rds", package = "poisspatial")) %>%
    ps_sfcs_to_wgs84()

  poly <- readRDS(system.file("sf/poly.rds", package = "poisspatial")) %>%
    ps_sfcs_to_wgs84()

  cent.pt1 <- ps_sfcs_centroid(pt, sfc_names = "geometry")
  cent.pt <- ps_sfcs_centroid(x = pt)
  cent.poly <- ps_sfcs_centroid(poly)
  cent.1 <- ps_sfcs_centroid(poly, sfc_names = "geometry")

  expect_true(inherits(cent.pt$geometry, "sfc_POINT"))
  expect_true(inherits(cent.poly$geometry, "sfc_POINT"))
  expect_true(inherits(cent.1$geometry, "sfc_POINT"))

  expect_true(inherits(cent.pt, "sf"))
  expect_true(inherits(cent.poly, "sf"))
  expect_true(inherits(cent.1, "sf"))

  expect_identical(length(cent.pt), 1L)
  expect_identical(length(cent.poly), 1L)
  expect_identical(length(cent.1), 1L)

  expect_identical(sf::st_crs(cent.pt), sf::st_crs(pt))
  expect_identical(sf::st_crs(cent.poly), sf::st_crs(poly))
  expect_identical(sf::st_crs(cent.1), sf::st_crs(pt))

  pt <- ps_sfcs_to_utm(pt)
  poly <- ps_sfcs_to_utm(poly)

  cent.pt <- ps_sfcs_centroid(x = pt)
  cent.poly <- ps_sfcs_centroid(poly)
  cent.1 <- ps_sfcs_centroid(poly, sfc_names = "geometry")

  expect_true(inherits(cent.pt$geometry, "sfc_POINT"))
  expect_true(inherits(cent.poly$geometry, "sfc_POINT"))
  expect_true(inherits(cent.1$geometry, "sfc_POINT"))

  expect_true(inherits(cent.pt, "sf"))
  expect_true(inherits(cent.poly, "sf"))
  expect_true(inherits(cent.1, "sf"))

  expect_identical(length(cent.pt), 1L)
  expect_identical(length(cent.poly), 1L)
  expect_identical(length(cent.1), 1L)

  expect_identical(sf::st_crs(cent.pt), sf::st_crs(pt))
  expect_identical(sf::st_crs(cent.poly), sf::st_crs(poly))
  expect_identical(sf::st_crs(cent.1), sf::st_crs(pt))
})

test_that("centroid1", {

  pt <- readRDS(system.file("sf/pt.rds", package = "poisspatial")) %>%
    ps_sfcs_to_wgs84()

  pt$color[2:3] <- "indigo"

  cent.pt <- ps_sfc_centroid1(pt)
  expect_true(inherits(cent.pt$geometry, "sfc_POINT"))
  expect_true(inherits(cent.pt, "sf"))
  expect_identical(sf::st_crs(cent.pt), sf::st_crs(pt))
  expect_identical(colnames(cent.pt), "geometry")
  expect_equal(ps_sfc_to_coords(cent.pt)$X, -117.0649, tolerance = 0.00001)
  expect_equal(ps_sfc_to_coords(cent.pt)$Y, 50.0058, tolerance = 0.000001)

  cent.pt <- ps_sfc_centroid1(pt, by = "color")
  expect_true(inherits(cent.pt$geometry, "sfc_POINT"))
  expect_true(inherits(cent.pt, "sf"))
  expect_identical(sf::st_crs(cent.pt), sf::st_crs(pt))
  expect_identical(colnames(cent.pt), c("color", "geometry"))
  expect_equal(ps_sfc_to_coords(cent.pt)$X, c(-117.0646, -117.0651), tolerance = 0.000001)
  expect_equal(ps_sfc_to_coords(cent.pt)$Y, c(50.00692, 50.00524), tolerance = 0.000001)

  cent.pt <- ps_sfc_centroid1(pt, nearest = TRUE)
  expect_true(inherits(cent.pt$geometry, "sfc_POINT"))
  expect_true(inherits(cent.pt, "sf"))
  expect_identical(sf::st_crs(cent.pt), sf::st_crs(pt))
  expect_identical(colnames(cent.pt), "geometry")
  expect_identical(ps_sfc_to_coords(cent.pt)$X, ps_sfc_to_coords(pt)$X[2])
  expect_identical(ps_sfc_to_coords(cent.pt)$Y, ps_sfc_to_coords(pt)$Y[2])

  cent.pt <- ps_sfc_centroid1(pt, by = "color", nearest = TRUE)
  expect_true(inherits(cent.pt$geometry, "sfc_POINT"))
  expect_true(inherits(cent.pt, "sf"))
  expect_identical(sf::st_crs(cent.pt), sf::st_crs(pt))
  expect_identical(colnames(cent.pt), c("color", "geometry"))
  expect_identical(ps_sfc_to_coords(cent.pt)$X, ps_sfc_to_coords(pt)$X[1:2])
  expect_identical(ps_sfc_to_coords(cent.pt)$Y, ps_sfc_to_coords(pt)$Y[1:2])

  pt <- ps_sfcs_to_utm(pt)
  cent.pt <- ps_sfc_centroid1(pt, by = "color", nearest = TRUE)
  expect_true(inherits(cent.pt$geometry, "sfc_POINT"))
  expect_true(inherits(cent.pt, "sf"))
  expect_identical(sf::st_crs(cent.pt), sf::st_crs(pt))
  expect_identical(colnames(cent.pt), c("color", "geometry"))
  expect_identical(ps_sfc_to_coords(cent.pt)$X, ps_sfc_to_coords(pt)$X[1:2])
  expect_identical(ps_sfc_to_coords(cent.pt)$Y, ps_sfc_to_coords(pt)$Y[1:2])
})
