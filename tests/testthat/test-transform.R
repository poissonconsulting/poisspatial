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

  # still works when in_crs and out_crs are NULL (default)
  ps_batch_transform(in_dir = "in/", out_dir = "out2/")
  l <- list.files("out2")
  expect_true(!identical(length(l), 0L))
  expect_true(any(grep(".sqlite", l)))
  # still works when forward slash missing
  ps_batch_transform(in_dir = "in/", out_dir = "out3")
  l <- list.files("out3")
  expect_true(!identical(length(l), 0L))
  expect_true(any(grep(".sqlite", l)))

  unlink("in", recursive = T)
  unlink("out", recursive = T)
  unlink("out2", recursive = T)
  unlink("out3", recursive = T)

})
