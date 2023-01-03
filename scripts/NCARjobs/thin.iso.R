library(sf)
library(tidyverse)
library(spThin)
library(car)
library(MASS)
library(UBL)

setwd("E:/chapter3/GEDI_FESM")
# setwd("/glade/scratch/kjfuller/data/chapter3")
thinfun = function(x){
  setwd("E:/chapter3/for GAMs")
  g = st_read(paste0("ch3_forGAMs_poly_prefire", x, "_final.gpkg"))
  g = g |>
    filter(fire_reg != 7 & fire_reg != 9)
  g$fire_reg = as.factor(g$fire_reg)
  targetcrs = st_crs(g)
  st_geometry(g) = NULL
  
  g$winddiff.iso = cos((g$aspect - g$maxwd) * pi / 180)
  g$winddiff.bom = cos((g$aspect - g$winddir) * pi / 180)
  
  g = g |> 
    filter(prog < 202)
  g$breaks = apply(g |> dplyr::select(firelines, roads), 1, FUN = min, na.rm = T)
  g$breaks.all2 = apply(g |> dplyr::select(firelines, roads, water2), 1, FUN = min, na.rm = T)
  g$breaks.all3 = apply(g |> dplyr::select(firelines, roads, water2, water3), 1, FUN = min, na.rm = T)
  g$breaks.all4 = apply(g |> dplyr::select(firelines, roads, water2, water3, water4), 1, FUN = min, na.rm = T)
  
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
                  winddir:windgust,
                  winddiff.iso,
                  winddiff.bom,
                  aspect,
                  firelines:water4,
                  breaks:breaks.all4,
                  house.density)
  g$fire_reg = as.factor(g$fire_reg)
  g = na.omit(g)
  
  memory.limit(size=50000)
  smote <- SmoteClassif(prog ~ .,
                        g,
                        dist = "HEOM", C.perc = "balance", k = 3)
  write.csv(smote, paste0("ch3_forGAMs_poly_prefire", x, "_smote.csv"), row.names = F)
}

thinfun(7)
thinfun(14)
thinfun(30)
thinfun(60)
thinfun(90)
thinfun(180)
