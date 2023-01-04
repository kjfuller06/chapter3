library(sf)
library(tidyverse)
library(spThin)
library(car)
library(MASS)
library(UBL)

# setwd("/glade/scratch/kjfuller/data/chapter3")
thinfun = function(x){
  setwd("E:/chapter3/for GAMs")
  g = st_read(paste0("ch3_forGAMs_poly_prefire", x, "_final.gpkg"))
  g$fire_reg = as.factor(g$fire_reg)
  targetcrs = st_crs(g)
  st_geometry(g) = NULL
  
  g = g %>%
    dplyr::select(prog,
                  fire_reg,
                  rh98:fhd_normal,
                  # slope,
                  # slope_min,
                  # slope_max,
                  maxtemp:maxwd,
                  ffdi_final,
                  ffdi_cat,
                  # ID,
                  elevation_sd,
                  stringybark,
                  stringybark.1,
                  stringybark.9,
                  # stringybark_min,
                  # stringybark_max,
                  ribbonbark,
                  ribbonbark.1,
                  ribbonbark.9,
                  # ribbonbark_min,
                  # ribbonbark_max,
                  LFMC,
                  LFMC.1,
                  LFMC.9,
                  # LFMC_min,
                  # LFMC_max,
                  VPD,
                  VPD.1,
                  VPD.9,
                  # VPD_min,
                  # VPD_max,
                  winddir,
                  wind.stdev,
                  windspeed,
                  windspeed.1,
                  windspeed.9,
                  # windspeed_min,
                  # windspeed_max,
                  windgust,
                  windgust.9,
                  # windgust_max,
                  maxtemp,
                  maxrh,
                  maxws,
                  winddiff.iso,
                  winddiff.bom,
                  firelines,
                  roads,
                  water2,
                  water3,
                  water4,
                  breaks,
                  breaks.all2,
                  breaks.all3,
                  breaks.all4,
                  house.density)
  g$fire_reg = as.factor(g$fire_reg)
  g = na.omit(g)
  
  g |> group_by(ffdi_cat) |> tally()
  C.perc = list("low" = 2, "high" = 1.5, "very high" = 0.9, "severe +" = 1) 
  memory.limit(size=50000)
  smote <- SmoteClassif(ffdi_cat ~ .,
                        g,
                        dist = "HEOM", C.perc = "balance", k = 3)
  smote <- SmoteClassif(prog ~ .,
                        smote,
                        dist = "HEOM", C.perc = "balance", k = 3)
  write.csv(smote, paste0("ch3_forGAMs_poly_prefire", x, "_smote.csv"), row.names = F)
}

thinfun(7)
thinfun(14)
thinfun(30)
thinfun(60)
thinfun(90)
thinfun(180)
