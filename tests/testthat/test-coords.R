context("coords")

test_that("ps_coords_to_sfc when missnig values", {
  x <- data.frame(X = c(1,1,10), Y = c(1,10,1))
  x$Row <- 2:4

  y <- ps_coords_to_sfc(x, crs = 28992, activate = FALSE)
  expect_identical(y, structure(list(Row = 2:4, geometry = structure(list(structure(c(1,
1), class = c("XY", "POINT", "sfg")), structure(c(1, 10), class = c("XY",
"POINT", "sfg")), structure(c(10, 1), class = c("XY", "POINT",
"sfg"))), class = c("sfc_POINT", "sfc"), precision = 0, bbox = structure(c(xmin = 1,
ymin = 1, xmax = 10, ymax = 10), class = "bbox"), crs = structure(list(
    epsg = 28992L, proj4string = "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.4171,50.3319,465.5524,-0.398957,0.343988,-1.87740,4.0725 +units=m +no_defs"), class = "crs"), n_empty = 0L)), row.names = c(NA,
-3L), class = "data.frame"))

  x$X[3] <- NA
  y2 <- ps_coords_to_sfc(x, crs = 28992, activate = FALSE)
  expect_identical(y[1:2,], y2[1:2,])
  expect_identical(y2$geometry[[2]], structure(c(1, 10), class = c("XY", "POINT", "sfg")))
  expect_identical(y2$geometry[[3]], structure(c(NA_real_, NA_real_), class = c("XY", "POINT", "sfg"
)))
})

