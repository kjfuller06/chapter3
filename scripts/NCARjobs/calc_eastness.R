library(raster)

setwd("/glade/scratch/kjfuller/data")
aspect = raster("proj_dem_aspect_30m.tif")

eastness = aspect
eastness = sin(eastness * pi / 180)
writeRaster(eastness, "proj_dem_eastness_30m.tif", overwrite = T)