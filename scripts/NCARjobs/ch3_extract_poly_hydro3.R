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
# setwd("E:/chapter3/water")
r = raster("distancetoHydroLine_nonpere_relevance3.tif")

g = st_transform(g, crs = st_crs(r))
l = raster::extract(r, g, method = 'simple')
g$hydro3 = unlist(lapply(l, FUN = function(x) quantile(x, probs = 0.5, na.rm = T)))
g$hydro3.1 = unlist(lapply(l, FUN = function(x) quantile(x, probs = 0.1, na.rm = T)))
g$hydro3.9 = unlist(lapply(l, FUN = function(x) quantile(x, probs = 0.9, na.rm = T)))

setwd("/glade/scratch/kjfuller/data/chapter3")
g = st_transform(g, crs = targetcrs)
st_write(g, paste0("ch3_forGAMs_poly_prefire180_hydro3_nonpere.gpkg"), delete_dsn = T)