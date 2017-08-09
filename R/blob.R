#' BLOB
#'
#' @param dir A string of the directory
#' @param ext A string of the file extension(s)
#' @param n An integer of the (maximal) number of records to be read.
#' @seealso readBin
#' @return A tibble with the columns FileName and BLOB
#' @export
blob <- function(dir, ext = "[.]xlsx$", n =  10000L) {
  check_string(dir)
  check_string(ext)
  check_count(n)

  if (!dir.exists(dir))
    stop("directory '", dir, "' does not exist", call. = FALSE)

  files <- list.files(dir, pattern = ext, full.names = TRUE, recursive = TRUE)

  if (!length(files))
    return(tibble::tibble(FileName = character(0), BLOB = integer(0)))

  sub <- sub(dir, "", files)
  sub <- dirname(files)

  filenames <- basename(files)
  filenames <- sub(ext, "", filenames)

  blob <- lapply(files, function(x) {
    readBin(con = x, what = integer(), n = n, size = NA_integer_, signed = TRUE,
            endian = "little")
  })

  tibble::tibble(File = filenames,
                 Sub = sub,
                BLOB = I(lapply(blob, function(x) {serialize(x, NULL)})))
}
