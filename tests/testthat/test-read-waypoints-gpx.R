context("read-waypoints-gpxs")

test_that("works", {

  wpts <- ps_read_waypoints_gpxs(system.file("gpx_wpt", package = "poisspatial"), recursive = TRUE)

  expect_is(wpts, "sf")
  expect_identical(names(wpts), c("File", "Waypoint", "geometry"))
  expect_identical(nrow(wpts), 42L)
  expect_identical(sort(unique(wpts$File)), sort(c("wptgpx1.gpx", "wptgpx2.gpx", "sub2/wptgpx3.gpx")))
})

