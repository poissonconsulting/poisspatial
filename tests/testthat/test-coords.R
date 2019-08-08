context("coords")

test_that("ps_coords_to_sfc when missnig values", {
  x <- data.frame(X = c(1,1,10), Y = c(1,10,1))
  x$Row <- 2:4

  y <- ps_coords_to_sfc(x, crs = 28992, activate = FALSE)

  x$X[3] <- NA
  y2 <- ps_coords_to_sfc(x, crs = 28992, activate = FALSE)
  expect_identical(y[1:2,], y2[1:2,])
  expect_identical(y[,1], y2[,1])
  expect_identical(y2$geometry[[2]], structure(c(1, 10), class = c("XY", "POINT", "sfg")))
  expect_identical(y2$geometry[[3]], structure(c(NA_real_, NA_real_), class = c("XY", "POINT", "sfg"
  )))
})
