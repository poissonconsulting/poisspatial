#' Nearest
#'
#' Inner join on two data.frames based on the nearest value in by.
#'
#' @param x A data.frame with the column by
#' @param y A data.frame.
#' @param by The unquoted name of the column to calculate the inner join by.
#'
#' @return xx
#' @export
ps_nearest <- function(x, y, by) {
  check_string(by)
  check_cols(x, by)
  check_cols(y, by)

  if (any(has_name(x, c("..index", "..metric"))))
    stop("x must not contain column '..index' or '..metric'", call. = FALSE)

  if (any(has_name(y, c("..index", "..metric"))))
    stop("y must not contain column '..index' or '..metric'", call. = FALSE)

  if (has_name(x, "Distance"))
    warning("column 'Distance' in x was replaced", call. = FALSE)

  x <- as.data.table(x)
  y <- as.data.table(y)

  y[, "..index" := 1:nrow(y)]
  x[,"..index" := y[x, "..index", on = by, roll = "nearest"]]
  setnames(y, old = by, new = "..metric")
  y <- y[x$..index]
  y[, "..index" := NULL]
  x[, "..index" := NULL]

  x <- cbind(x, y)

  x$Distance <- x[,by,with = FALSE][[1]] - x$..metric
  x[, "..metric" := NULL]
  tibble::as_tibble(x)
}
