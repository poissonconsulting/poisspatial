epsg <- readRDS(file.path(getwd(), "data-raw/epsg.rds"))
usethis::use_data(epsg)
