library(terra)
library(sf)
library(raster)
library(viridis)

setwd("D:/chapter3/climatedem")
r <- rast("dem.map")
# r_agg = aggregate(r, fact = 1000, cores = 4)
# plot(r_agg)
# writeRaster(r_agg, "dem_agg1000.tif", overwrite = T)

# r_agg = raster("dem_agg1000.tif")
# r_poly = rasterToPolygons(r_agg, na.rm = F)
# r_poly = st_as_sf(r_poly)
# r_poly = st_buffer(r_poly, dist = 1)
# 
# plot(r)
# plot(st_geometry(r_poly), add = T)
# st_write(r_poly, "dem_agg1000.gpkg", delete_dsn = T)

r_vect = vect("dem_agg1000.gpkg")
tiles = makeTiles(r, y = r_vect, na.rm = T, overwrite = T)
tiles

r2 = vrt(tiles)

plot(r2)
