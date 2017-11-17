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

#' Load spatial files
#'
#' @param dir A string of the geodatabase directory.
#' @param layers A character string vector indicating which layers to load.
#' @param crs Default crs of layer if missing.
#' @param rename A function that is used to rename files (after removing extension .csv) before they are passed to make.names.
#' @param envir The environment to assign the data frames.
#' @param fun A function that is applied to all sf objects before they are assigned to envir.
#' @param ... Additional arguments passed to \code{st_read}.
#' @return An invisible character vector of the layer names.
#' @export
ps_load_gdb <- function(dir = "~/Poisson/Data/spatial/fwa/gdb/FWA_BC.gdb", layers = NULL, crs = NULL, rename = identity,
                            envir = parent.frame(), fun = identity, ...) {

  check_string(dir)
  if(!is_crs(crs)) ps_error("must provide a valid crs.")
  if (!is.function(rename)) ps_error("rename must be a function")
  if (!is.function(fun)) ps_error("fun must be a function")
  if (!dir.exists(dir)) ps_error(dir, "' does not exist.")
  if(tools::file_ext(dir) != "gdb") ps_error("dir must have extension .gdb")

  l <- sf::st_layers(dir)$name
  if(is.null(layers)){layers <- l} else {layers <- layers}
  # if(!l %in% layers) ps_error("Layers do not exist in geodatabase.")

  g <- purrr::map(layers, ~ tryCatch(sf::st_read(dsn = dir, layer = .), error = function(e) NULL))

  names(g) <- layers %>%
    rename() %>%
    make.names(unique = TRUE)

  # set crs
  if(!is.null(crs)){
    g %<>% purrr::map(function(x){
      if(is.na(sf::st_crs(x))){
        x %<>% sf::st_set_crs(crs)} else {
          x <- x
        }
    })
  }

  g %<>% purrr::map(fun)

  purrr::imap(g, function(x, name) {assign(name, x, envir = envir)})
  invisible(l)
}

#' FWA gdbs
#'
#' @param dir A character string indicating path to directory holding fwa geodatabases.
#' @return A factor of the geodatabase names.
#' @export
ps_fwa_gdbs <- function(dir = "~/Poisson/Data/spatial/fwa/gdb") {
  if (!dir.exists(dir)) ps_error("directory '", dir, "' does not exist.")
  x <- list.files(dir, full.names = F, recursive = F, pattern = ".gdb")
  x
}

#' FWA layers
#'
#' @param dir A character string indicating path to directory holding fwa geodatabases.
#' @param gdb A character string vector indicating FWA geodatabases to extract layers from.
#' The default should not have to be changed.
#' @return A factor of the layer names within specified geodatabase.
#' @export
ps_fwa_layers <- function(gdb = "FWA_BC.gdb", dir = "~/Poisson/Data/spatial/fwa/gdb/") {
  check_string(gdb[1])
  if (!dir.exists(dir)) ps_error("directory '", dir, "' does not exist.")
  if(all(!gdb %in% ps_fwa_gdbs())) ps_error("That is not a recognised fwa geodatabase. See ps_fwa_gdbs() for options.")
  x <- purrr::map(gdb, ~ sf::st_layers(dsn = paste0(dir, .))[[1]]) %>%
    unlist %>%
    unique
  x
}

#' FWA shortcuts
#'
#' @param dir A character string indicating path to directory holding fwa geodatabases.
#' @param gdb A character string vector indicating FWA geodatabases to extract layer shortcuts from.
#' @return A factor of the layer names within specified geodatabase.
#' @export
ps_fwa_shortcuts <- function(gdb = "FWA_BC.gdb", dir = "~/Poisson/Data/spatial/fwa/gdb/") {
  if(all(!gdb %in% ps_fwa_gdbs())) ps_error("gdb is not a valid geodatabase.")
  x <- ps_fwa_layers(gdb = gdb)
  ex <- grep("_max|_fwa|50K|CODES", x, value = T)
  x <- setdiff(x, ex) %>%
    tolower %>%
    gsub("fwa_|_poly|_sp", "", .)
  x
}

#' Read a FWA layer.
#'
#' Internal function that uses shared Poisson dropbox folder.

#' @param shortcut A character string indicating shortcut name of layer to read (layer stripped of 'FWA_' and '_POLY' or '_SP').
#' Any value of shortcut other than NULL will override layer argument.
#' @param dir A character string indicating path to directory holding fwa geodatabases.
#' @param gdb A character string indicating which geodatabase to read. See ps_fwa_gdbs() for options.
#' @param layer A character string indicating which layer to read. See ps_fwa_layers() for options.
#' @return sf object.
#' @export
ps_read_fwa <- function(shortcut = NULL, gdb = "FWA_BC.gdb",
                        layer = "FWA_COASTLINES_SP", dir = "~/Poisson/Data/spatial/fwa/gdb/") {

  if(length(gdb) != 1L) ps_error("Please select one geodatabase to read.")
  if(length(layer) != 1L) ps_error("Please select one layer to read.")
  check_string(layer)
  check_string(gdb)
  if (!dir.exists(dir)) ps_error("directory '", dir, "' does not exist.")
  if(!gdb %in% ps_fwa_gdbs()) ps_error("That is not a recognised fwa geodatabase. See ps_fwa_gdbs() for options.")
  if(all(!layer %in% ps_fwa_layers(gdb = gdb))) ps_error("That layer does not exist in the specified geodatabase. See ps_fwa_layers() for options.")

  if(!is.null(shortcut)){
    check_string(shortcut)
    if(!shortcut %in% ps_fwa_shortcuts(gdb = ps_fwa_gdbs())) ps_error("Shortcut is not valid. See ps_fwa_shortcuts() for options.")
    layer <- c(paste0("fwa_", shortcut, "_poly"), paste0("fwa_", shortcut, "_sp")) %>%
    toupper
    all <- ps_fwa_layers(gdb = ps_fwa_gdbs())
    layer <- layer[layer %in% all]} else {layer <- layer}

  x <- sf::st_read(dsn = paste0(dir, gdb), layer = layer)
  x
}

#' Watershed Codes
#'
#' @return A factor of the 13 watershed codes for the Kootenay Region of British Columbia.
#' @export
ps_ws_codes <- function() {
  x <- ps_fwa_shortcuts(gdb = "FWA_LINEAR_BOUNDARIES_SP.gdb") %>%
    toupper %>%
    factor
  x
}



