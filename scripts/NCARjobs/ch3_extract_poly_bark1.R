library(sf)
library(raster)
library(tidyverse)
library(exactextractr)
library(rgeos)

# # load isochrons 
# setwd("/glade/scratch/kjfuller/data/chapter3")
# # setwd("E:/chapter3/for GAMs")
# g = st_read("ch3_forGAMs_poly_prefire180_structure.gpkg")
# targetcrs = st_crs(g)
# g = g |> 
#   dplyr::select(ID)
# 
# setwd("/glade/scratch/kjfuller/data")
# # setwd("D:/chapter1/bark-type-SDM/outputs/RFs/final models/final geotiffs")
# r = raster("NSW_stringybark_distribution.tif")
# 
# g = st_transform(g, crs = st_crs(r))
# g$stringybark = raster::extract(r, g, method = 'simple', fun = median, na.rm = T)
# 
# setwd("/glade/scratch/kjfuller/data/chapter3")
# g = st_transform(g, crs = targetcrs)
# st_write(g, paste0("ch3_forGAMs_poly_prefire180_bark1.gpkg"), delete_dsn = T)

# load isochrons ####
setwd("/glade/scratch/kjfuller/data/chapter3")
# setwd("E:/chapter3/for GAMs")
g = st_read("ch3_forGAMs_poly_prefire180_bark1.gpkg")
targetcrs = st_crs(g)
g = g |> 
  dplyr::select(ID,
                stringybark)

setwd("/glade/scratch/kjfuller/data")
# setwd("D:/chapter1/bark-type-SDM/outputs/RFs/final models/final geotiffs")
r = raster("NSW_stringybark_distribution.tif")

g = st_transform(g, crs = st_crs(r))
g$stringybark_max = raster::extract(r, g, method = 'simple', fun = max, na.rm = T)
g$stringybark_min = raster::extract(r, g, method = 'simple', fun = min, na.rm = T)

setwd("/glade/scratch/kjfuller/data/chapter3")
g = st_transform(g, crs = targetcrs)
st_write(g, paste0("ch3_forGAMs_poly_prefire180_bark1.2.gpkg"), delete_dsn = T)