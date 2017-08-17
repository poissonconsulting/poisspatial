context("nearest")

test_that("nearest", {

  x <- data.frame(Date = ISOdate(2000, 1, c(1, 4, 9)))
  y <- data.frame(Date = ISOdate(2000, 1, c(2, 6, 12)))
  y$Row <- 1:nrow(x)

  n <- ps_nearest(x, y, "Date", dist_col = "Distance")
  expect_is(n, "data.frame")
  expect_identical(colnames(n), c("Date", "Row", "Distance"))
  expect_identical(n$Date, x$Date)
  expect_identical(n$Row, c(1L, 1L, 2L))
  expect_identical(n$Distance, as.difftime(c(-1, 2, 3), units = "days"))
})

test_that("nearest", {

  x <- data.frame(Date = ISOdate(2000, 1, c(1, 4, 9)))
  y <- data.frame(Date = ISOdate(2000, 1, c(5, 6, 12)))
  y$Row <- 1:nrow(x)

  n <- ps_nearest(x, y, "Date", dist_col = "Distance")
  expect_is(n, "data.frame")
  expect_identical(colnames(n), c("Date", "Row", "Distance"))
  expect_identical(n$Date, x$Date)
  expect_identical(n$Row, c(1L, 1L, 2L))
  expect_identical(n$Distance, as.difftime(c(-4, -1, 3), units = "days"))
})

test_that("nearest", {

  x <- data.frame(Date = ISOdate(2000, 1, c(9, 1, 4)))
  y <- data.frame(Date = ISOdate(2000, 1, c(12, 5, 6)))
  y$Row <- 1:nrow(x)

  n <- ps_nearest(x, y, "Date", dist_col = "Distance")
  expect_is(n, "data.frame")
  expect_identical(colnames(n), c("Date", "Row", "Distance"))
  expect_identical(n$Date, x$Date)
  expect_identical(n$Row, c(3L, 2L, 2L))
  expect_identical(n$Distance, as.difftime(c(3, -4, -1), units = "days"))
})

