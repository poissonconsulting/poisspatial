context("load")

test_that("can load multiple spatial files and files of a certain type", {
  dir <- system.file("files", package = "poisspatial")

  files <- ps_load_spatial(dir, recursive = T, pattern = ".sqlite")
  expect_identical(length(files), 1L)

  files <- ps_load_spatial(dir, recursive = T, pattern = ".shp")
  expect_identical(length(files), 2L)
  expect_true(is.na(sf::st_crs(polyna)))

  files <- ps_load_spatial(dir, recursive = T, pattern = NULL)
  expect_identical(length(files), 3L)
})

test_that("can set crs when missing", {
  dir <- system.file("files", package = "poisspatial")
  files <- ps_load_spatial(dir, recursive = T, pattern = ".shp", crs = 26911)
  expect_true(!is.na(sf::st_crs(polyna)))
  expect_identical(sf::st_crs(polyna)$epsg, 26911L)
})

test_that("rename and fun arguments work", {
  dir <- system.file("files", package = "poisspatial")
  rename <- function(x) {gsub("pol", "", x)}
  trans <- function(x) {x %<>% sf::st_transform(4326)}
  files <- ps_load_spatial(dir, recursive = T, pattern = ".shp", crs = 26911, rename = rename, fun = trans)
  expect_identical(sf::st_crs(yna)$epsg, 4326L)
})

# test_that("geodatabase loads", {
#
# })
