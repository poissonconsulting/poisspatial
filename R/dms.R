#' Decimal Degrees to Degrees and Decimal Minutes.
#'
#' @param x A numeric vector.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' ps_dd2ddm(c(70.654, NA, -56.654))
ps_dd2ddm <- function(x) {
  chk_vector(x)
  check_values(x, c(-180, 180, NA))
  negative <- x < 0
  x %<>% abs()
  d <- floor(x)
  dm <- (x - d) * 60
  d[!is.na(negative) & negative] <- d[!is.na(negative) & negative] * -1
  x <- paste(d, dm)
  x[is.na(negative)] <- NA
  x
}

#' Decimal Minutes to Minutes and Decimal Seconds.
#'
#' @param x A numeric vector.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' ps_dm2mds(c(10.654, NA))
ps_dm2mds <- function(x) {
  chk_vector(x)
  check_values(x, c(0, 60, NA))
  negative <- x < 0
  m <- floor(x)
  ds <- (x - m) * 60
  x <- paste(m, ds)
  x[is.na(negative)] <- NA
  x
}

#' Decimal Degrees to Degrees and Decimal minutes.
#'
#' @param x A numeric vector.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' ps_dd2dmds(c(70.654, NA, -56.654))
ps_dd2dmds <- function(x) {
  chk_vector(x)
  check_values(x, c(-180, 180, NA))
  negative <- x < 0
  x %<>% abs()
  d <- floor(x)
  dm <- (x - d) * 60
  mds <- ps_dm2mds(dm)
  d[!is.na(negative) & negative] <- d[!is.na(negative) & negative] * -1
  x <- paste(d, mds)
  x[is.na(negative)] <- NA
  x
}

#' Degrees and Decimal Minutes to Decimal Degrees
#'
#' @param x A character vector.
#'
#' @return A numeric vector.
#' @export
#'
#' @examples
#' ps_ddm2dd(c("70 39.24", NA, "-56 39.24"))
ps_ddm2dd <- function(x) {
  chk_vector(x)
  check_values(x, c("", NA))
  x %<>% strsplit(" ")
  length <- vapply(x, length, 1L)
  if (any(length > 2)) {
    ps_error("ddm must be two real numbers separated by a single space")
  }

  negative <- vapply(x, function(x) grepl("^-", x)[1], TRUE)
  is.na(negative) <- vapply(x, function(x) is.na(x[1]), TRUE)
  x %<>% lapply(function(x) sub("^-", "", x)) %>%
    lapply(as.numeric)
  x[!is.na(negative)] %<>% lapply(function(x) x[1] + x[2] / 60)
  x %<>% unlist()
  x[!is.na(negative) & negative] %<>% magrittr::multiply_by(-1)
  x
}
