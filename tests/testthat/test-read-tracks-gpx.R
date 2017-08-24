context("read-tracks-gpx")

test_that("works", {

  dir <- system.file("gpx", package = "poisspatial")

  tracks <- ps_read_tracks_gpx(file.path(dir, "20110401.gpx"), tz = "PST8PDT")

  expect_identical(class(tracks), c("sf", "tbl_df", "tbl", "data.frame"))
  expect_identical(names(tracks), c("datetime", "geometry"))
  expect_identical(nrow(tracks), 9644L)
  expect_identical(lubridate::tz(tracks$datetime), "PST8PDT")
  expect_identical(lubridate::hour(tracks$datetime[1]), 21L)
})


