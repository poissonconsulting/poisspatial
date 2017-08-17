#' Nearest Join
#'
#' Each row in object x is bound with a closet row in object y based on the closest values in by.
#'
#' @param x The object.
#' @param y The object with rows to bind to each row in y.
#' @param by A character vector of length 1 or 2 specifying the columns to use when calculating distance.
#' @param dist_col A string indicating the name of the column to save the distance in.
#' @param ... Not used
#' @return The updated object with the columns in y.
#' @export
ps_nearest <- function(x, y, by = c("X", "Y"), dist_col = NULL, ...) {
  UseMethod("ps_nearest")
}

nearest <- function(x, y) {
  stopifnot(is.data.frame(x))
  stopifnot(is.data.frame(y))

  stopifnot(nrow(x) > 0)
  stopifnot(nrow(y) > 0)

  stopifnot(ncol(x) %in% c(1L, 2L))
  stopifnot(!anyDuplicated(colnames(x)))
  stopifnot(identical(colnames(x), colnames(y)))
  stopifnot(!any(c("..index", "..metric", "..distance") %in% colnames(x)))

  if (ncol(x) == 1L) {
    x <- as.data.table(x)
    y <- as.data.table(y)

    y[, "..index" := 1:nrow(y)]
    x[,"..index" := y[x, "..index", on = colnames(x), roll = "nearest"]]
    y <- y[x$..index]
    setnames(y, old = colnames(y)[1], new = "..metric")

    x[, "..index" := NULL]
    x <- cbind(x, y)

    x$..distance <- x[,colnames(x)[1],with = FALSE][[1]] - x$..metric
    x[, c("..index", "..distance")]
    return(x)
  }
  stop()
}

#' @export
ps_nearest.data.frame <- function(x, y, by = c("X", "Y"), dist_col = NULL, ...) {
  x <- as.data.frame(x)
  y <- as.data.frame(y)

  x2 <- nearest(x[by], y[by])
  y <- y[!colnames(y) %in% by]
  rownames <- rownames(x)
  x <- cbind(x, y[x2$..index,,drop = FALSE], ..distance = x2$..distance)
  rownames(x) <- rownames
  if (!is.null(dist_col)) x[[dist_col]] <- x[["..distance"]]
  x$..distance <- NULL
  x
}

#' @export
ps_nearest.tbl_df <- function(x, y, by = c("X", "Y"), dist_col = NULL, ...) {
  x %<>%
    as.data.frame() %>%
    ps_nearest(y = y, by = by, dist_col = dist_col) %>%
    tibble::as_tibble()
  print(x)
  x
}
