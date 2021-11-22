test_that("ps_nearest_feature joins correct feature, manages names and calculates distance", {

pt <- sf::st_read(system.file("gpkg/points.gpkg", package = "poisspatial"))
poly <- sf::st_read(system.file("gpkg/polygons.gpkg", package = "poisspatial"))

pt2 <- ps_nearest_feature(pt, poly, dist_col = "dist")

expect_equal(as.numeric(pt2$dist), c(0.0000000, 0.4665229))
expect_identical(pt2$ID, c("b", "a"))
expect_identical(names(pt2), c("Number", "ID", "dist", "geometry", "geometry.y"))
expect_identical(ps_active_sfc_name(pt2), "geometry")

pt2 <- ps_nearest_feature(pt, poly, dist_col = "ID")
expect_identical(names(pt2), c("Number", "ID.y", "ID", "geometry", "geometry.y"))

poly_ab <- poly[1:2, ]
poly_cd <- poly[3:4, ]

poly2 <- ps_nearest_feature(poly_cd, poly_ab, dist_col = "dist")

expect_equal(as.numeric(poly2$dist), c(0.8908796, 5.8361786))
expect_identical(poly2$ID.y, c("b", "a"))
expect_identical(names(poly2), c("ID", "ID.y", "dist", "geometry", "geometry.y"))
expect_identical(ps_active_sfc_name(pt2), "geometry")

})


test_that("ps_nearest_feature errors", {

pt <- sf::st_read(system.file("gpkg/points.gpkg", package = "poisspatial"))
poly <- sf::st_read(system.file("gpkg/polygons.gpkg", package = "poisspatial"))

expect_error(ps_nearest_feature(poly, pt, dist_col = "ID"),
             "`dist_col` must not already be present in `names(x)`", fixed = TRUE)

expect_error(ps_nearest_feature(poly, pt %>% tibble::as_tibble(), dist_col = "ID"),
             "`x` and `y` must both be sf objects.")

})
