library(terra)
library(sf)
library(raster)
library(viridis)
library(tmap)

setwd("D:/chapter3/climatedem")
tiles = list.files(pattern = "drainage")
tiles = tiles[grepl(".map$", tiles)]
tiles

t1 = rast(tiles[1])
nrow(t1)*ncol(t1) == length(values(t1))
## TRUE
tmap_mode("view")
tm_shape(t1) + tm_raster()

topleft = 1
topright = ncol(t1)
bottomleft = length(values(t1)) - ncol(t1) + 1
bottomright = length(values(t1))

top = c(topleft:topright)
bottom = c(bottomleft:bottomright)
left = seq(topleft, bottomleft, by = ncol(t1))
right = seq(topright, bottomright, by = ncol(t1))

values(t1)[top] = NA
values(t1)[bottom] = NA
values(t1)[left] = NA
values(t1)[right] = NA

tmap_mode("view")
tm_shape(t1) + tm_raster()

l = list()
for(i in c(1:length(tiles))){
  t1 = rast(tiles[i])
  
  topleft = 1
  topright = ncol(t1)
  bottomleft = length(values(t1)) - ncol(t1) + 1
  bottomright = length(values(t1))
  
  top = c(topleft:topright)
  bottom = c(bottomleft:bottomright)
  left = seq(topleft, bottomleft, by = ncol(t1))
  right = seq(topright, bottomright, by = ncol(t1))
  
  values(t1)[top] = NA
  values(t1)[bottom] = NA
  values(t1)[left] = NA
  values(t1)[right] = NA
  
  l[[i]] = t1
}
l$fun <- mean
rast.mosaic <- do.call(mosaic, l)
writeRaster(rast.mosaic, "drainage_mosaic.tif", overwrite = T)

rast.mosaic = rast("drainage_mosaic.tif")

setwd("D:/chapter1/data")
nsw = st_read("NSW_sans_islands.shp") |> 
  st_transform(crs = st_crs(rast.mosaic))

rast.clip = mask(rast.mosaic, nsw)
plot(rast.clip)

setwd("D:/chapter3/climatedem")
writeRaster(rast.clip, "drainage_mosaic_clip.tif", overwrite = T)
