test_that("elevation data can be added to a specified sfc column", {
  ptz <- readRDS(system.file("sf/ptz.rds", package = "poisspatial"))
  # check that error when no active sfc name and sfc_column not specified
  expect_error(ptz %<>% ps_sfc_add_z())
  # check remove_z arg
  ptz2 <- ps_sfc_add_z(ptz, sfc_column = "geometry")
  expect_true(ncol(ptz2) == 5)
  ptz2 <- ps_sfc_add_z(ptz, sfc_column = "geometry", remove_z = F)
  expect_true(ncol(ptz2) == 6)
  # check works on default args when activate sfc
  ptz2 <- ps_activate_sfc(ptz) %>%
    ps_sfc_add_z()
  # check that sfc is xyz
  expect_true(inherits(ptz2$geometry[[1]], "XYZ"))
  expect_true(!inherits(ptz$geometry[[1]], "XYZ"))
  # must have correct x_column
  expect_error(ptz %<>% rename(Elev = Elvation) %>%
    ps_sfc_add_z())
  # check works if units not set
  pt <- readRDS(system.file("sf/pt.rds", package = "poisspatial"))
  pt$Elevation <- c(1, 100, 200)
  pt %<>% ps_sfc_add_z()
  expect_true(ncol(ptz2) == 5)
  expect_true(inherits(pt$geometry[[1]], "XYZ"))
})
