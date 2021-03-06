test_that("works", {

  tracks <- ps_read_tracks_gpxs(system.file("gpx", package = "poisspatial"), tz = "PST8PDT", recursive = TRUE)

  expect_is(tracks, "sf")
  expect_identical(names(tracks), c("File", "Track", "DateTime", "geometry"))
  expect_identical(nrow(tracks), 29306L)
  expect_identical(lubridate::tz(tracks$DateTime), "PST8PDT")
  expect_identical(lubridate::hour(tracks$DateTime[1]), 21L)
  expect_identical(sort(unique(tracks$File)), sort(c("20110401.gpx", "20110402.gpx", "sub2/20110403.gpx")))
})


