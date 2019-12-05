
# library(sf)
#
#
# ni_shp <- "../rict_spec/ni_shape.zip"
# ni <- unzip(ni_shp)
# ni <- st_read(ni[7])
#
# sfc <- sf::st_sfc(st_point(c(311651, 453001)))
# sf <- st_sf(a = 1, geom = sfc)
# st_crs(sf) <- 29902
#
# test <- ni[st_nearest_feature(sf[1, ], ni), ]
