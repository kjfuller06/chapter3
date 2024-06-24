library(sf)
library(raster)
library(tidyverse)
library(exactextractr)
library(rgeos)

# load isochrons ####
# setwd("/glade/scratch/kjfuller/data/chapter3")
setwd("E:/chapter3/for GAMs")
g = st_read("ch3_forGAMs_poly_prefire180_structure_redo_2-14.gpkg")
targetcrs = st_crs(g)

# load rasters
# setwd("/glade/scratch/kjfuller/data")
setwd("D:/chapter1/data")

# elevation ####
print("reading elevation file")
r = raster("proj_dem_s.tif")

print("converting to elevation crs")
g = st_transform(g, crs = st_crs(r))
print("extracting elevation")
elevation = raster::extract(r, g, method = 'simple')
g$elevation_sd = unlist(lapply(elevation, FUN = function(x) sd(x, na.rm = T)/sqrt(length(x))))
g$elevation_md = unlist(lapply(elevation, median, na.rm = T))

# setwd("/glade/scratch/kjfuller/data/chapter3")
setwd("E:/chapter3/for GAMs")
g = st_transform(g, crs = targetcrs)
# st_write(g, paste0("ch3_forGAMs_poly_prefire180_elevation.gpkg"), delete_dsn = T)
st_write(g, paste0("ch3_forGAMs_poly_prefire180_elevation_redo_6-19.gpkg"), delete_dsn = T)
