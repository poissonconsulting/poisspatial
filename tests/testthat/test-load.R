context("load")

test_that("can load multiple spatial files", {
  dir <- system.file("files", package = "poisspatial")

  files <- ps_load_spatial(dir, recursive = T, pattern = ".sqlite")
  expect_identical(length(files), 1L)

  files <- ps_load_spatial(dir, recursive = T, pattern = ".shp")
  expect_identical(length(files), 2L)

  files <- ps_load_spatial(dir, recursive = T, pattern = NULL)

})

test_that("can load sqlite files only", {

  dir <- system.file("files", package = "poisspatial")
  files <- ps_load_spatial(dir, recursive = T, pattern = ".sqlite")



})

test_that("rename and fun arguments work", {

  rename <- function(x) {tolower(gsub("kas", "", x))}
  trans <- function(x) {x %<>% sf::st_transform(4326)}

})
