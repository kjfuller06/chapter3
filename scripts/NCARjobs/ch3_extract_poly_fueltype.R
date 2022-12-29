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

setwd("/glade/scratch/kjfuller/data")
# setwd("D:/chapter1/bark-type-SDM/data")
fire_reg = read.csv("fire_regimes.csv") |> 
  dplyr::select(fueltype,
                fire_reg)

# fire regimes ####
# setwd("D:/chapter1/bark-type-SDM/data")
r = raster("fuels_30m.tif")

g = st_transform(g, crs = st_crs(r))
fueltype = raster::extract(r, g, method = 'simple')

getmode = function(x){
  uniqv <- unique(unlist(x))
  uniqv = uniqv[!is.na(uniqv)]
  m = uniqv[which.max(tabulate(match(x, uniqv)))]
  return(m)
} 

g$fueltype = unlist(lapply(fueltype, getmode))
g = g |> 
  left_join(fire_reg)

setwd("/glade/scratch/kjfuller/data/chapter3")
g = st_transform(g, crs = targetcrs)
st_write(g, paste0("ch3_forGAMs_poly_prefire180_fueltype.gpkg"), delete_dsn = T)