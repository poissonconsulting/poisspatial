#' Batch transform
#'
#' Batch transform spatial files. Function finds all files with specified extension pattern within input directory
#' and sets crs if does not already exist. Files are projected to new crs and written
#' to output directory with specified extension and file format (allows any extension recognised by sf::st_write function).
#'
#' @param in_dir A string of the path to the directory containing spatial files.
#' @param out_dir A string of the path to the directory that output files will be saved to.
#' @param recursive A logical specifying whether to search directory recursively.
#' @param in_crs An integer of the EPSG that files are currently in (only set if crs not currently already present).
#' @param out_crs An integer of the EPSG that files will be transformed to.
#' @param in_pattern A string of the pattern to use when searching for input files.
#' @param out_extension A string of the extension specifying which file format output files will be saved to.
#' @return Files with specified crs and extension written to output directory.
#' @export
ps_batch_transform <- function(in_dir, out_dir,
                                 recursive = T,
                                 in_pattern = "[.]shp$",
                                 out_extension = ".sqlite",
                                 in_crs, out_crs) {




  if(!(dir.exists(out_dir))) {
    dir.create(out_dir)
  }

  filex <- list.files(path = in_dir, full.names = T, recursive = recursive, pattern = in_pattern)

  purrr::walk(filex, function(x){

    tryCatch({
      namex <- basename(x) %>%
        sub(in_pattern, out_extension, .)

      x %<>% sf::st_read(quiet = T)

      if(is.na(sf::st_crs(x))) {
        x %<>% sf::st_set_crs(in_crs)
      } else{}

      x %<>% sf::st_transform(out_crs)

      writex <- paste0(out_dir, namex)
      if(!(file.exists(writex))){
        sf::st_write(x, dsn = path.expand(writex), quiet = T)
      }
    }, error = function(e){cat("Error:", conditionMessage(e), "\n")})

  })
}
