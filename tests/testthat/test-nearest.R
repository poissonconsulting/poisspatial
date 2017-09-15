context("nearest")

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

test_that("nearest data.table 1", {

  x <- data.table::data.table(Date = ISOdate(2000, 1, c(1, 4, 9)))
  y <- data.frame(Date = ISOdate(2000, 1, c(5, 6, 12)))
  y$Row <- 1:nrow(x)

  n <- ps_nearest(x, y, "Date", dist_col = "Distance")
  expect_is(n, "data.frame")
  expect_identical(class(n), c("data.table", "data.frame"))
  expect_identical(n$Date, x$Date)
  expect_identical(n$Row, c(1L, 1L, 2L))
  expect_identical(n$Distance, c(345600, 86400, 259200))
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
  expect_equal(n$D, c(0, 6.103278, 5.830952), tolerance = 0.000001)
})

test_that("nearest sf", {
  x <- data.frame(X = c(1,1,10), Y = c(1,10,1))
  y <- data.frame(X = c(1,4.5,5,6), Y = c(1,5,4,6))

  y$Row <- 1:nrow(y)

  x <- sf::st_as_sf(x, coords = c("X", "Y"), crs = 28992)
  y <- sf::st_as_sf(y, coords = c("X", "Y"), crs = 28992)

  x <- ps_rename_sf_column_name(x, "GEOMETRY")
  y <- ps_rename_sf_column_name(y, "GEOMETRY2")

  n <- ps_nearest(x, y, dist_col = "D")
  expect_identical(class(n), c("sf", "data.frame"))
  expect_identical(colnames(n), c("Row", "D", "geometry", "geometry.y"))
  expect_identical(n$geometry, x$GEOMETRY)
  expect_equal(n$D, c(0, 6.103278, 5.830952), tolerance = 0.000001)
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
  expect_equal(n$D, c(0, 6.103278, 5.830952), tolerance = 0.000001)
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
  expect_equal(n$D, c(1.00000, 5.00000, 94.13288), tolerance = 0.00001)
})

