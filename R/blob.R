#' BLOB
#'
#' @param dir A string of the directory
#' @param ext A string of the file extension(s)
#' @param n An integer of the (maximal) number of records to be read.
#' @param recursive A flag indicating whether no recurse into subdirectories.
#' @seealso readBin
#' @return A tibble with the columns FileName and BLOB
#' @export
blob <- function(dir, ext = "[.]pdf$", n =  10000L, recursive = TRUE) {
  .Deprecated("ps_blob")
  ps_blob(dir, ext = ext, n = n, recursive = recursive)
}

#' BLOB
#'
#' @param dir A string of the directory
#' @param ext A string of the file extension(s)
#' @param n An integer of the (maximal) number of records to be read.
#' @param recursive A flag indicating whether no recurse into subdirectories.
#' @seealso readBin
#' @return A tibble with the columns FileName and BLOB
#' @export
ps_blob <- function(dir, ext = "[.]pdf$", n =  10000L, recursive = TRUE) {
  check_string(dir)
  check_string(ext)
  check_count(n)
  check_flag(recursive)

  if (!dir.exists(dir))
    stop("directory '", dir, "' does not exist", call. = FALSE)

  files <- list.files(dir, pattern = ext, full.names = TRUE, recursive = recursive)

  if (!length(files))
    return(tibble::tibble(FileName = character(0), BLOB = integer(0)))

  sub <- list.files(dir, pattern = ext, recursive = recursive)
  sub <- dirname(sub)

  filenames <- basename(files)

  blob <- lapply(files, function(x) {
    readBin(con = x, what = integer(), n = n, size = NA_integer_, signed = TRUE,
            endian = "little")
  })

  tibble::tibble(FileName = filenames,
                 SubDirectory = sub,
                BLOB = I(lapply(blob, function(x) {serialize(x, NULL)})))
}
