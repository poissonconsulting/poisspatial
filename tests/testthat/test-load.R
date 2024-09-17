test_that("can load multiple spatial files and files of a certain type", {
  dir <- system.file("files", package = "poisspatial")

  sink("/dev/null")
  files <- ps_load_spatial(dir, recursive = T, pattern = ".sqlite")
  sink()
  expect_identical(length(files), 1L)

  sink("/dev/null")
  files <- ps_load_spatial(dir, recursive = T, pattern = ".shp")
  sink()
  expect_identical(length(files), 2L)
  expect_true(is.na(sf::st_crs(polyna)))

  sink("/dev/null")
  files <- ps_load_spatial(dir, recursive = T, pattern = NULL)
  sink()
  expect_identical(length(files), 4L)
})

test_that("can set crs when missing", {
  dir <- system.file("files", package = "poisspatial")
  sink("/dev/null")
  files <- ps_load_spatial(dir, recursive = T, pattern = ".shp", crs = 26911)
  sink()
  expect_true(!is.na(sf::st_crs(polyna)))
  expect_identical(sf::st_crs(polyna)$epsg, 26911L)
})

test_that("rename and fun arguments work", {
  dir <- system.file("files", package = "poisspatial")
  rename <- function(x) {gsub("pol", "", x)}
  trans <- function(x) {x %<>% sf::st_transform(4326)}
  sink("/dev/null")
  files <- ps_load_spatial(dir, recursive = T, pattern = ".shp", crs = 26911, rename = rename, fun = trans)
  sink()
  expect_identical(sf::st_crs(yna)$epsg, 4326L)
})

test_that("spatial database loads", {
  path <- system.file("files/ikeda.gpkg", package = "poisspatial")
  rename <- toupper
  trans <- function(x) {sf::st_transform(x, 3005)}
  sink("/dev/null")
  ikeda <- ps_load_spatial_db(path = path, rename = rename, fun = trans)
  sink()
  expect_identical(sf::st_crs(SITE)$proj4string, "+proj=aea +lat_0=45 +lon_0=-126 +lat_1=50 +lat_2=58.5 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs")
  l <- ls()
  expect_true(any(l %in% c("CREEK", "SITE", "IKEDA")))
})
