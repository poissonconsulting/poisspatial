#' Load spatial files
#'
#' @param dir A string of the directory.
#' @param pattern An optional regular expression. Only file names which match the regular expression will be returned.
#' If NULL, all files with extensions readable by st_read wlll be returned.
#' @param recursive A flag indicating whether to include files in subdirectories.
#' @param crs Default crs of object if missing.
#' @param rename A function that is used to rename files (after removing extension .csv) before they are passed to make.names.
#' @param envir The environment to assign the data frames.
#' @param fun A function that is applied to all sf objects before they are assigned to envir.
#' @param ... Additional arguments passed to \code{st_read}.
#' @return An invisible character vector of the file names.
#' @export
ps_load_spatial <- function(dir = ".", pattern = NULL, recursive = FALSE,
                            crs = NULL, rename = identity,
                            envir = parent.frame(), fun = identity, ...) {

  check_string(dir)
  if(!is.null(pattern)){check_string(pattern)}
  check_flag(recursive)
  if(!is_crs(crs)) ps_error("must provide a valid crs.")

  if (!is.function(rename)) ps_error("rename must be a function")
  if (!is.function(fun)) ps_error("fun must be a function")

  if (!dir.exists(dir)) ps_error("directory '", dir, "' does not exist")

  file_names <- list.files(dir, pattern = pattern, full.names = TRUE,
                           recursive = recursive)

   if (!length(file_names)) {
    poisutils::ps_warning("no files matching that extension found.")
    return(invisible(character(0)))
  }

  df <- data.frame(file_names,
                   files = basename(file_names))
  df$ext <- tools::file_ext(df$files)
  df %<>% purrr::modify(as.character)

  df <- df[!df$ext %in% c("sbn", "dbf", "sbx", "shx", "xlsx", "csv", "pdf", "docx", "prj"),]

  df$names <- purrr::map2(df$files, df$ext, function(x, y){
    x %<>% gsub(paste0(".", y), "", .)
  })

  data <- purrr::map(df$file_names, function(x){
    tryCatch(sf::st_read(x), error = function(e) NULL)
  })

  names(data) <- df$names %>%
    rename() %>%
    make.names(unique = TRUE)

  data[sapply(data, is.null)] <- NULL

  # set crs
  if(!is.null(crs)){
    data %<>% purrr::map(function(x){
      if(is.na(sf::st_crs(x))){
        x %<>% sf::st_set_crs(crs)} else {
          x <- x
        }
    })
  }

  data %<>% purrr::map(fun)

  purrr::imap(data, function(x, name) {assign(name, x, envir = envir)})
  invisible(df$files)
}

#' Watershed Codes
#'
#' @return A factor of the 13 watershed codes for the Kootenay Region of British Columbia.
#' @export
#' @examples
#' ps_ws_codes()
ps_ws_codes <- function() {
  x <- c("BULL", "CLRH", "COLR", "DUNC", "ELKR", "KHOR",
         "KOTL", "KOTR", "LARL",  "REVL", "SLOC", "SMAR", "UARL")
  x %<>% factor
  x
}

#' Read a FWA layer.
#'
#' Internal function that uses shared Poisson dropbox folder.
#'
#' @param layer A character string indicating which dataset to read. Options are "lakes", "rivers", "watersheds", "riverpoly"
#' @param path A character string indicating path to FWA data.
#' @return sf object.
#' @export
ps_read_fwa <- function(layer, path = "~/Poisson/Data/spatial/fwa/") {

  check_string(layer)
  if (!dir.exists(path)) ps_error("directory '", path, "' does not exist")

  pathfwa <- paste0(path, layer, ".sqlite")
  sf <- sf::st_read(pathfwa)
  sf
}

#' Filter/clip a FWA dataset.
#'
#' If ws_code argument not used, this is simply a wrapper on st_intersection.
#'
#' @param x A sf object to subset.
#' @param ws_codes A character vector specifying the watershed codes.
#' @param clip A sf/sfc object to clip dataset to.
#' @return sf object.
#' @export
ps_subset_fwa <- function(x, ws_codes = c(
  "BULL", "CLRH", "COLR", "DUNC", "ELKR", "KHOR",
  "KOTL", "KOTR", "LARL",  "REVL", "SLOC", "SMAR", "UARL"), clip = NULL) {

  if(!is.sf(x)) ps_error("x must be a sf object (e.g. from ps_read_fwa() function.")
  check_string(ws_codes)

  if (!all(ws_codes %in% ps_ws_codes()))
    stop("permitted wscodes are: ", punctuate_strings(ps_ws_codes(), "and"))

  x <- x[x$WTRSHDGRPC %in% ws_codes,]

  if(!is.null(clip)) {
    clip <- sf::st_geometry(clip)
    if(!is.sfc(clip)) ps_error("clip must be a sf or sfc object.")
    x %<>% sf::st_intersection(clip)
  } else {x <- x}
  x
}

