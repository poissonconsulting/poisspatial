
# cant be run on actions
# test_that("ps_elevation_google", {
#   x <- data.frame(X = c(1,1,10), Y = c(1,10,1))
#   x$Row <- 2:4
#
#   x <- ps_coords_to_sfc(x, crs = 28992, activate = FALSE)
#   x <- ps_activate_sfc(x)
#
#   e <- ps_elevation_google(x)
#   expect_identical(colnames(e), c("Row", "geometry", "Z"))
#   expect_identical(e[c("Row", "geometry")], x[c("Row", "geometry")])
#   expect_equal(e$Z, c(135.154434204102, 135.121871948242, 135.305969238281),
#                tolerance = 1e-03)
# })

test_that("ps_elevation_google invalid key", {
  x <- data.frame(X = c(1,1,10), Y = c(1,10,1))
  x$Row <- 2:4

  x <- ps_coords_to_sfc(x, crs = 28992, activate = FALSE)
  x <- ps_activate_sfc(x)

  expect_error(ps_elevation_google(x, key = "rubbish"))
})

# test_that("ps_elevation_google missing values", {
#   x <- data.frame(X = c(1,1,10), Y = c(1,NA,1))
#   x$Row <- 2:4
#
#   x <- ps_coords_to_sfc(x, crs = 28992, activate = FALSE)
#   x <- ps_activate_sfc(x)
#
#   e <- ps_elevation_google(x)
#   expect_identical(colnames(e), c("Row", "geometry", "Z"))
#   expect_identical(e[c("Row", "geometry")], x[c("Row", "geometry")])
#   expect_equal(e$Z, c(135.154434204102, NA, 135.305969238281),
#                tolerance = 1e-03)
# })
