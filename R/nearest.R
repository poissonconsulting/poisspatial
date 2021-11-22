#' Nearest Neighbour
#'
#' Each row in object x is bound with its closest neighbour in object y.
#' Uses the nabor package.
#'
#' The column(s) to use when calculating the distances are converted to numeric values.
#' Missing values are currently not permitted.
#'
#' sf objects have their sf (active geometry) column renamed to geometry.
#'
#' @param x A data.frame, tibble or sf object.
#' @param y A data.frame, tibble or sf object.
#' @param by A possibly named character vector specifying the column(s) to calculate the distance over.
#' @param dist_col A string indicating the name of the column to save the distance in.
#' @param ... Not used
#' @export
ps_nearest <- function(x, y, by = c("X", "Y"), dist_col = NULL, ...) {
  UseMethod("ps_nearest")
}

# data 2 numeric matrix
d2nm <- function(x, by) {
  x <- x[, by, drop = FALSE]
  x[] <- lapply(x, as.numeric)
  x %<>% as.matrix()
  x
}

nn1 <- function(x, y) {
  stopifnot(is.matrix(x))
  stopifnot(is.matrix(y))

  stopifnot(is.numeric(x))
  stopifnot(is.numeric(y))

  stopifnot(nrow(x) > 0)
  stopifnot(nrow(y) > 0)

  stopifnot(ncol(x) > 0)
  stopifnot(ncol(y) == ncol(x))

  stopifnot(!any(is.na(x)))
  stopifnot(!any(is.na(y)))

  nn1 <- nabor::knn(y, query = x, k = 1L)
  names(nn1) <- c("index", "distance")
  nn1 %<>% as_data_frame()
  nn1
}

#' @export
ps_nearest.data.frame <- function(x, y, by = c("X", "Y"), dist_col = NULL, ...) {
  chk_vector(by)
  check_values(by, "")
  chk_gte(by, 1)
  chk_unique(by)
  chk_null_or(dist_col, vld = vld_string)
  if (!is.null(names(by))) {
    chk_unique(names(by))
    bx <- names(by)
    names(by) <- NULL
  } else
    bx <- by

  if (is.sf(y))
    y %<>% ps_rename_active_sfc()

  x %<>% as_data_frame()
  y %<>% as_data_frame()

  check_names(x, bx)
  check_names(y, by)
  check_data(x)
  check_data(y)

  mx <- d2nm(x, bx)
  my <- d2nm(y, by)

  colnames(my) <- colnames(mx)

  nn1 <- nn1(mx, my)

  y <- y[nn1$index,]

  if (!is.null(dist_col)) y[dist_col] <- nn1$distance

  rownames <- rownames(x)
  x %<>% cbind(y)
  colnames(x)[duplicated(colnames(x))] %<>% paste0(".y")
  rownames(x) <- rownames
  x
}

#' @export
ps_nearest.tbl_df <- function(x, y, by = c("X", "Y"), dist_col = NULL, ...) {
  if (is.sf(y))
    y %<>% ps_rename_active_sfc()

  x %<>%
    as_data_frame() %>%
    ps_nearest(y = y, by = by, dist_col = dist_col) %>%
    tibble::as_tibble()
  x
}

#' @export
ps_nearest.sf <- function(x, y, by = c("X", "Y"), dist_col = NULL, ...) {
  x %<>% ps_rename_active_sfc()
  if (is.sf(y))
    y %<>% ps_rename_active_sfc()

  colnames <- c(colnames(x), colnames(y))

  if (is.sf(y)) y %<>% sf::st_transform(sf::st_crs(x))

  x %<>%
    as_data_frame() %>%
    ps_nearest(y = y, by = by, dist_col = dist_col)

  colnames[duplicated(colnames)] %<>% paste0(".y")

  if (!is.null(dist_col)) {
    colnames %<>% setdiff(dist_col)
    colnames %<>% c(dist_col)
  }

  x <- x[colnames]

  x %<>% sf::st_as_sf()
  x
}

#' Nearest Feature
#'
#' Each row in object x is bound with the closest feature in object y.
#' Uses sf::st_nearest_feature.
#'
#' sf objects have their sf (active geometry) column renamed to geometry.
#'
#' @param x An sf object.
#' @param y An sf object.
#' @param dist_col A string indicating the name of the column to save the distance in.
#' @param ... Not used
#' @export
ps_nearest_feature <- function(x, y, dist_col = NULL, ...) {

  check_data(x)
  check_data(y)
  if(!(is.sf(x) & is.sf(y))) err("`x` and `y` must both be sf objects.")
  chk_null_or(dist_col, vld = vld_string)


  x %<>% ps_rename_active_sfc()
  y %<>% ps_rename_active_sfc()
  y %<>% sf::st_transform(sf::st_crs(x))

  if(ps_active_sfc_name(y) %in% names(x)){
    names(y)[names(y) == ps_active_sfc_name(y)] <- paste0(ps_active_sfc_name(y), ".y")
    st_geometry(y) <- paste0(ps_active_sfc_name(y), ".y")
  }

  y <- y[st_nearest_feature(x, y), ]

  if(!is.null(dist_col)){
    if(dist_col %in% names(x)) err("`dist_col` must not already be present in `names(x)`")
    x[dist_col] <- st_distance(x, y, by_element = TRUE)
  }

  while(any(duplicated(c(names(x), names(y))))) {
    names(y)[names(y) %in% names(x)] %<>% paste0(".y")
  }

  x %<>% cbind(y)

  sfc_names <- names(x)[sapply(names(x), function(colname) {is.sfc(x[colname][[1]])})]
  colnames <- c(names(x)[!names(x) %in% c(sfc_names, dist_col)], dist_col, sfc_names)
  x <- x[colnames]
  x %<>% sf::st_as_sf()

}
