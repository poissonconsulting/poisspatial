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
  check_vector(x, c(-180, 180, NA))
  negative <- x < 0
  x %<>% abs()
  d <- floor(x)
  dm <- (x - d) * 60
  d[!is.na(negative) & negative] <- d[!is.na(negative) & negative] * -1
  x <- paste0(d, "\u00B0 ", dm ,"'")
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
  check_vector(x, c(0, 60, NA))
  negative <- x < 0
  m <- floor(x)
  ds <- (x - m) * 60
  x <- paste0(m, "' ", ds , "\u0022")
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
  check_vector(x, c(-180, 180, NA))
  negative <- x < 0
  x %<>% abs()
  d <- floor(x)
  dm <- (x - d) * 60
  mds <- ps_dm2mds(dm)
  d[!is.na(negative) & negative] <- d[!is.na(negative) & negative] * -1
  x <- paste0(d, "\u00B0 ", mds)
  x[is.na(negative)] <- NA
  x
}
