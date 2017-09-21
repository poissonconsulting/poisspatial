context("transform")

test_that("batch transform", {
  x <- data.frame(X = c(1,1,10), Y = c(1,10,1))
  x$Row <- 2:4
  x <- sf::st_as_sf(x, coords = c("X", "Y"), crs = 28992)
  dir.create("in")
  sf::st_write(x, "in/x.shp")
  ps_batch_transform(in_dir = "in/", out_dir = "out/", in_crs = 28992, out_crs = 4326)
  l <- list.files("out")
  expect_true(!identical(length(l), 0L))
  expect_true(any(grep(".sqlite", l)))
  z <- sf::st_read("out/x.sqlite")
  expect_true(is.sf(z))
  expect_true(sf::st_crs(z)$epsg == 4326)
  unlink("in", recursive = T)
  unlink("out", recursive = T)
})
