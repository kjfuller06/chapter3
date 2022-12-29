library(sf)
library(raster)
library(tidyverse)
library(exactextractr)
library(rgeos)

# load isochrons ####
setwd("/glade/scratch/kjfuller/data/chapter3")
# setwd("E:/chapter3/for GAMs")
g = st_read("ch3_forGAMs_poly_prefire180_structure.gpkg")
targetcrs = st_crs(g)
g = g |> 
  dplyr::select(ID)

setwd("/glade/scratch/kjfuller/data")
# setwd("D:/chapter1/bark-type-SDM/outputs/RFs/final models/final geotiffs")
r = raster("NSW_ribboning_distribution.tif")

g = st_transform(g, crs = st_crs(r))
g$ribbonbark = raster::extract(r, g, method = 'simple', fun = median, na.rm = T)

setwd("/glade/scratch/kjfuller/data/chapter3")
g = st_transform(g, crs = targetcrs)
st_write(g, paste0("ch3_forGAMs_poly_prefire180_bark2.gpkg"), delete_dsn = T)