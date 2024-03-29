test_that("nearest data.frame 1", {

  x <- data.frame(Date = ISOdate(2000, 1, c(1, 4, 9)))
  y <- data.frame(Date = ISOdate(2000, 1, c(2, 6, 12)))
  y$Row <- 1:nrow(x)
  y$Row2 <- y$Row + 1.5

  n <- ps_nearest(x, y, "Date")
  expect_identical(class(n), c("data.frame"))
  expect_identical(colnames(n), c("Date", "Date.y", "Row", "Row2"))
  expect_identical(n$Date, x$Date)
  expect_identical(n$Row, c(1L, 1L, 2L))
  expect_identical(n$Row2, c(1L, 1L, 2L) + 1.5)
})

test_that("nearest tbl_df 1", {

  x <- tibble::tibble(Date = ISOdate(2000, 1, c(9, 1, 4)))
  y <- data.frame(Date = ISOdate(2000, 1, c(12, 5, 6)))
  y$Row <- 1:nrow(x)

  n <- ps_nearest(x, y, "Date", dist_col = "Distance")
  expect_identical(class(n), c("tbl_df", "tbl", "data.frame"))
  expect_identical(colnames(n), c("Date", "Date.y", "Row", "Distance"))
  expect_identical(n$Date, x$Date)
  expect_identical(n$Row, c(1L, 2L, 2L))
  expect_identical(n$Distance, c(259200, 345600, 86400))
})

test_that("nearest data.frame 2", {
  x <- data.frame(X = c(1,1,10), Y = c(1,10,1))
  y <- data.frame(X = c(1,4.5,5,6), Y = c(1,5,4,6))

  n <- ps_nearest(x, y, dist_col = "D")
  expect_identical(class(n), c("data.frame"))
  expect_identical(colnames(n), c("X", "Y", "X.y", "Y.y", "D"))
  expect_identical(n$X, x$X)
  expect_identical(n$Y, x$Y)
  expect_equal(n$D, c(0, 6.10327780786685, 5.8309518948453))
})

test_that("nearest sf", {
  x <- data.frame(X = c(1,1,10), Y = c(1,10,1))
  y <- data.frame(X = c(1,4.5,5,6), Y = c(1,5,4,6))

  y$Row <- 1:nrow(y)

  x <- sf::st_as_sf(x, coords = c("X", "Y"), crs = 28992)
  y <- sf::st_as_sf(y, coords = c("X", "Y"), crs = 28992)

  x <- ps_rename_active_sfc(x, "GEOMETRY")
  y <- ps_rename_active_sfc(y, "GEOMETRY2")

  n <- ps_nearest(x, y, dist_col = "D")
  expect_identical(class(n), c("sf", "data.frame"))
  expect_identical(colnames(n), c("Row", "D", "geometry", "geometry.y"))
  expect_identical(n$geometry, x$GEOMETRY)
  expect_equal(n$D, c(0, 6.10327780786685, 5.8309518948453))

})

test_that("nearest sf with data.frame", {
  x <- data.frame(X = c(1,1,10), Y = c(1,10,1))
  y <- data.frame(X = c(1,4.5,5,6), Y = c(1,5,4,6))

  y$Row <- 1:nrow(y)

  x <- sf::st_as_sf(x, coords = c("X", "Y"), crs = 28992)

  n <- ps_nearest(x, y, dist_col = "D")
  expect_identical(class(n), c("sf", "data.frame"))
  expect_identical(colnames(n), c("X", "Y", "Row", "D", "geometry"))
  expect_identical(n$geometry, x$geometry)
  expect_equal(n$D, c(0, 6.10327780786685, 5.8309518948453))
})

test_that("nearest sf with X and Y and reprojection", {
  x <- data.frame(X = c(1,1,10), Y = c(1,10,1))
  y <- data.frame(X = c(1,4.5,5,6), Y = c(1,5,4,6))

  y$Row <- 1:nrow(y)

  x <- sf::st_as_sf(x, coords = c("X", "Y"), crs = 28992)
  y <- sf::st_as_sf(y, coords = c("X", "Y"), crs = 28992)

  y <- sf::st_transform(y, 4326)

  x$X <- c(0,3,100)

  n <- ps_nearest(x, y, dist_col = "D")
  expect_identical(class(n), c("sf", "data.frame"))
  expect_identical(colnames(n), c("X", "Row", "D", "geometry", "geometry.y"))
  expect_identical(n$geometry, x$geometry)
  expect_equal(n$D, c(0.999960142739643, 4.99990310533304, 94.1329294416483), tolerance = 1e-05)
})

test_that("ps_nearest issues warning for non-point sf objects", {

  pt <- sf::st_read(system.file("gpkg/points.gpkg", package = "poisspatial"))
  poly <- sf::st_read(system.file("gpkg/polygons.gpkg", package = "poisspatial"))

  expect_warning(ps_nearest(pt, poly), "Distance calculation uses nearest vertex for non-point geometries. Use `ps_nearest_feature` for calculating nearest feature boundary for lines and polygons.")

})
