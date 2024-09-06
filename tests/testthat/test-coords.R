test_that("ps_coords_to_sfc when missnig values", {
  x <- data.frame(X = c(1,1,10), Y = c(1,10,1))
  x$Row <- 2:4

  y <- ps_coords_to_sfc(x, crs = 28992, activate = FALSE)

  x$X[2] <- NA
  y2 <- ps_coords_to_sfc(x, crs = 28992, activate = FALSE)
  expect_identical(y[c(1,3),], y2[c(1,3),])
  expect_identical(y[,1], y2[,1])
  expect_identical(y2$geometry[[2]], structure(c(NA_real_, NA_real_), class = c("XY", "POINT", "sfg"
  )))
})

test_that("ps_coords_to_sfc retain_orig flag keeps input columns when TRUE", {
  x <- data.frame(X = c(1,1,10), Y = c(1,10,1))
  x$Row <- 2:4

  y <- ps_coords_to_sfc(x, crs = 28992, activate = FALSE, retain_orig = TRUE)

  expect_identical(x$X, y$X)
  expect_identical(x$Y, y$Y)
})

test_that("ps_coords_to_sfc retain_orig flag drops input columns when FALSE", {
  x <- data.frame(X = c(1,1,10), Y = c(1,10,1))
  x$Row <- 2:4

  y <- ps_coords_to_sfc(x, crs = 28992, activate = FALSE, retain_orig = FALSE)

  expect_identical(is.null(y$X), TRUE)
  expect_identical(is.null(y$Y), TRUE)
})

test_that("ps_sfc_to_coords retain_orig flag keeps input column when TRUE", {
  x <- data.frame(X = c(1,1,10), Y = c(1,10,1))
  x$Row <- 2:4

  y <- ps_coords_to_sfc(x, crs = 28992, activate = FALSE, retain_orig = TRUE)
  Y2 <- ps_sfc_to_coords(y[,"geometry"], sfc_name = "geometry", retain_orig = TRUE)

  expect_identical(y$geometry, Y2$geometry)
})

test_that("ps_sfc_to_coords retain_orig flag keeps input column when TRUE", {
  x <- data.frame(X = c(1,1,10), Y = c(1,10,1))
  x$Row <- 2:4

  y <- ps_coords_to_sfc(x, crs = 28992, activate = FALSE, retain_orig = FALSE)

  expect_identical(is.null(y$X), TRUE)
  expect_identical(is.null(y$Y), TRUE)
})
