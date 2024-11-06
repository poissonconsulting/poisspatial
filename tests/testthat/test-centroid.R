test_that("works", {
  pt <- readRDS(system.file("sf/pt.rds", package = "poisspatial")) %>%
    ps_sfcs_to_wgs84()

  poly <- readRDS(system.file("sf/poly.rds", package = "poisspatial")) %>%
    ps_sfcs_to_wgs84()

  cent.pt1 <- ps_sfcs_centroid(pt, sfc_names = "geometry")
  cent.pt <- ps_sfcs_centroid(x = pt)
  cent.poly <- ps_sfcs_centroid(poly)
  cent.1 <- ps_sfcs_centroid(poly, sfc_names = "geometry", union = FALSE)

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
  expect_equal(ps_sfc_to_coords(cent.pt)$X, -117.064942319467)
  expect_equal(ps_sfc_to_coords(cent.pt)$Y, 50.0058006516964)

  cent.pt <- ps_sfc_centroid1(pt, by = "color")
  expect_true(inherits(cent.pt$geometry, "sfc_POINT"))
  expect_true(inherits(cent.pt, "sf"))
  expect_identical(sf::st_crs(cent.pt), sf::st_crs(pt))
  expect_identical(colnames(cent.pt), c("color", "geometry"))

  expect_equal(ps_sfc_to_coords(cent.pt)$X, ps_sfc_to_coords(cent.pt)$X)
  expect_equal(ps_sfc_to_coords(cent.pt)$Y, c(50.006919103519, 50.0052414257851))

  cent.pt <- ps_sfc_centroid1(pt, nearest = TRUE)
  expect_true(inherits(cent.pt$geometry, "sfc_POINT"))
  expect_true(inherits(cent.pt, "sf"))
  expect_identical(sf::st_crs(cent.pt), sf::st_crs(pt))
  expect_identical(colnames(cent.pt), "geometry")
  expect_equal(ps_sfc_to_coords(cent.pt)$X, -117.064654556369)
  expect_equal(ps_sfc_to_coords(cent.pt)$Y, 50.0062715017257)

  cent.pt <- ps_sfc_centroid1(pt, by = "color", nearest = TRUE)
  expect_true(inherits(cent.pt$geometry, "sfc_POINT"))
  expect_true(inherits(cent.pt, "sf"))
  expect_identical(sf::st_crs(cent.pt), sf::st_crs(pt))
  expect_identical(colnames(cent.pt), c("color", "geometry"))
  expect_equal(sort(ps_sfc_to_coords(cent.pt)$X), c(-117.0655727986, -117.0645996034))
  expect_equal(sort(ps_sfc_to_coords(cent.pt)$Y), c(50.00421134984, 50.00691910352))

  pt <- rbind(pt, pt[pt$id == 2, ])

  pt <- ps_sfcs_to_utm(pt)
  cent.pt <- ps_sfc_centroid1(pt, by = "color")
  expect_true(inherits(cent.pt$geometry, "sfc_POINT"))
  expect_true(inherits(cent.pt, "sf"))
  expect_identical(sf::st_crs(cent.pt), sf::st_crs(pt))
  expect_identical(colnames(cent.pt), c("color", "geometry"))
  expect_equal(ps_sfc_to_coords(cent.pt)$X, c(495371.000000044, 495345.000000045))

  cent.pt <- ps_sfc_centroid1(pt, by = "color", nearest = TRUE)
  expect_true(inherits(cent.pt$geometry, "sfc_POINT"))
  expect_true(inherits(cent.pt, "sf"))
  expect_identical(sf::st_crs(cent.pt), sf::st_crs(pt))
  expect_identical(colnames(cent.pt), c("color", "geometry"))
  expect_equal(ps_sfc_to_coords(cent.pt)$X, c(495371.000000044, 495367.000000045))
})
