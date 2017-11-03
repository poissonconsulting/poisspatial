context("centroid")

test_that("works", {

  pt <- readRDS(system.file("sf/pt.rds", package = "poisspatial")) %>%
    ps_sfcs_to_wgs84()

  poly <- readRDS(system.file("sf/poly.rds", package = "poisspatial")) %>%
    ps_sfcs_to_wgs84()

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
