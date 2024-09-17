get_null_device <- function() {
  if (.Platform$OS.type == "windows") {
    return("nul")
  } else {
    return("/dev/null")
  }
}
