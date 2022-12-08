library(sf)
library(tidyverse)
library(spThin)

# setwd("E:/chapter3/GEDI_FESM")
setwd("/glade/scratch/kjfuller/data/chapter3")
thinfun = function(x){
  g = st_read(paste0("ch3_forGAMs_prefire", x, "_allvars.gpkg"))
  g = g |> 
    filter(!is.na(LFMC) & !is.na(stringybark) & !is.na(ribbonbark)) |> 
    filter(fire_reg != 7 & fire_reg != 9)
  g$fire_reg = as.factor(g$fire_reg)
  targetcrs = st_crs(g)
  g$lon = st_coordinates(g)[,1]
  g$lat = st_coordinates(g)[,2]
  st_geometry(g) = NULL
  
  g1 = aggregate(data = g, slope ~ ID, FUN = median)
  g2 = aggregate(data = g, northness ~ ID, FUN = median)
  g3 = aggregate(data = g, eastness ~ ID, FUN = median)
  g4 = aggregate(data = g, elevation ~ ID, FUN = function(x){standard deviation})
  
  g_thin = st_as_sf(g_thin, coords = c("lon", "lat"), crs = targetcrs)
  st_write(g_thin, paste0("ch3_forGAMs_prefire", x, "_thinned0.1.gpkg"))
}
thinfun(7)
thinfun(14)
thinfun(30)
thinfun(60)
thinfun(90)
thinfun(180)
