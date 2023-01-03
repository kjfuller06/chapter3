library(sf)
library(tidyverse)
library(spThin)
library(car)
library(MASS)
library(UBL)

# setwd("E:/chapter3/GEDI_FESM")
setwd("/glade/scratch/kjfuller/data/chapter3")
# thinfun = function(x){
  # setwd("E:/chapter3/for GAMs")
  g = st_read(paste0("ch3_forGAMs_poly_prefire", x, "_final.gpkg"))
  g = g |>
    filter(fire_reg != 7 & fire_reg != 9)
  g$fire_reg = as.factor(g$fire_reg)
  targetcrs = st_crs(g)
  st_geometry(g) = NULL
  
  g$winddiff = g$aspect - g$maxwd
  g$winddiff = cos(g$winddiff * pi / 180)
  
  g = g %>%
    dplyr::select(prog,
                  fire_reg,
                  rh98:fhd_normal,
                  slope,
                  maxtemp:maxwd,
                  ffdi_final,
                  # ID,
                  elevation_sd,
                  stringybark,
                  ribbonbark,
                  LFMC,
                  VPD,
                  # winddir:windgust,
                  winddiff,
                  aspect,
                  firelines:water4,
                  house.density,
                  lon,
                  lat)
  g$fire_reg = as.factor(g$fire_reg)
  g = na.omit(g)
  
  memory.limit(size=50000)
  smote <- SmoteClassif(prog ~ .,
                        g,
                        dist = "HEOM", C.perc = "balance", k = 3)
  smote = st_as_sf(smote, coords = c("lon", "lat"), crs = targetcrs)
  st_write(smote, paste0("ch3_forGAMs_poly_prefire", x, "_smote.gpkg"), delete_dsn = T)
# }
# 
# thinfun(7)
# thinfun(14)
# thinfun(30)
# thinfun(60)
# thinfun(90)
# thinfun(180)
