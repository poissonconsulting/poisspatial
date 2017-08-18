is.sf <- function(x) inherits(x, "sf")

as_data_frame <- function(x) {
  if (!is.sf(x))
    return(as.data.frame(x))

  x %<>% sf::st_cast(to = "POINT")
  coords <- sf::st_coordinates(x)

  x %<>%
    as.data.frame() %>%
    cbind(coords)
  colnames(x)[duplicated(colnames(x))] %<>% paste0(".2")
  x
}
