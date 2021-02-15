test_that("works", {

  pt <- readRDS(system.file("sf/pt.rds", package = "poisspatial"))
  expect_true(!ps_equal_crs(pt))
  pt %<>% ps_sfcs_to_wgs84()
  expect_true(ps_equal_crs(pt))
  expect_identical(ps_get_proj4string(pt$geometry), "+proj=longlat +datum=WGS84 +no_defs")

  poly <- readRDS(system.file("sf/poly.rds", package = "poisspatial")) %>%
    ps_sfcs_to_wgs84()

  cent <- ps_sfcs_centroid(pt)

  expect_is(ps_utm_note(cent), "character")
  expect_true(!is.na(ps_utm_note(cent)))
  expect_is(ps_utm_proj4string(cent), "character")
  expect_is(ps_utm_zone(cent), "numeric")

  expect_identical(length(ps_utm_note(pt)), length(pt$geometry))
  expect_identical(length(ps_utm_zone(pt)), length(pt$geometry))
  expect_identical(length(ps_utm_proj4string(pt)), length(pt$geometry))

  y <- ps_sfcs_to_crs(pt, crs = 26911)
  z <- ps_sfcs_to_crs(pt, sfc_names = "geometry", crs = 26911)

  expect_identical(sf::st_crs(y$geometry.2)$proj4string, sf::st_crs(z$geometry)$proj4string)
  expect_identical(sf::st_crs(y$geometry.2)$proj4string, sf::st_crs(y$geometry)$proj4string)

  x <- ps_sfcs_to_utm(pt)
  y <- ps_sfcs_to_utm(pt, sfc_names = "geometry.2")
  z <- ps_sfcs_to_utm(poly)

  expect_is(x, "sf")
  expect_is(y, "sf")
  expect_is(z, "sf")
  expect_is(y$geometry.2, "sfc")
  expect_is(z$geometry, "sfc")
  expect_is(y$geometry.2, "sfc_POINT")
  expect_is(z$geometry, "sfc_POLYGON")
})
