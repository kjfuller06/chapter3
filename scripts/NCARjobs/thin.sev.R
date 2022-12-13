library(sf)
library(tidyverse)
library(spThin)
library(car)
library(MASS)
library(UBL)

# setwd("E:/chapter3/GEDI_FESM")
setwd("/glade/scratch/kjfuller/data/chapter3")
thinfun = function(x){
  g = st_read(paste0("ch3_forGAMs_prefire", x, "_allvars2.gpkg"))
  g = g |>
    filter(!is.na(LFMC) & !is.na(stringybark) & !is.na(ribbonbark)) |>
    filter(fire_reg != 7 & fire_reg != 9)
  g$fire_reg = as.factor(g$fire_reg)
  targetcrs = st_crs(g)
  g$lon = st_coordinates(g)[,1]
  g$lat = st_coordinates(g)[,2]
  st_geometry(g) = NULL

  l = list()
  for(i in c(1:length(unique(g$fire_reg)))){
    g_temp = g |>
      filter(fire_reg == unique(g$fire_reg)[i])
    l[[i]] = as.data.frame(thin.algorithm(data.frame(long = g_temp$lon, lat = g_temp$lat), thin.par = 1, reps = 1))
    l[[i]] = g |>
      inner_join(l[[i]], by = c("lon" = "Longitude", "lat" = "Latitude"))
  }
  g_thin = bind_rows(l)
  g_thin = st_as_sf(g_thin, coords = c("lon", "lat"), crs = targetcrs)
  st_write(g_thin, paste0("ch3_forGAMs_prefire", x, "_thinned1.gpkg"), delete_dsn = T)
  
  g = st_read(paste0("ch3_forGAMs_prefire", x, "_thinned1.gpkg"))
  targetcrs = st_crs(g)
  g$lon = st_coordinates(g)[,1]
  g$lat = st_coordinates(g)[,2]
  st_geometry(g) = NULL
  g = g %>%
    dplyr::select(severity,
                  fire_reg,
                  rh98,
                  fhd_normal,
                  cover_z_1,
                  over_cover,
                  slope,
                  elevation,
                  northness,
                  eastness,
                  # TSF.hist,
                  ffdi_final,
                  FireTypeCategoryId,
                  LFMC,
                  VPD,
                  stringybark,
                  ribbonbark,
                  lon,
                  lat)
  g$fire_reg = as.factor(g$fire_reg)
  g$severity = as.factor(g$severity)
  g$FireTypeCategoryId = as.factor(g$FireTypeCategoryId)
  
  for(i in c(1:length(unique(g$severity)))){
    g1 = g |> 
      filter(severity == unique(g$severity)[i])
    g1$severity = 1
    g2 = g |> 
      filter(severity != unique(g$severity)[i])
    g2$severity = 0
    g1 = rbind(g1, g2)
    
    memory.limit(size=50000)
    smote <- SmoteClassif(severity ~ .,
                          g1,
                          dist = "HEOM", C.perc = "balance", k = 3)
    smote = st_as_sf(smote, coords = c("lon", "lat"), crs = targetcrs)
    st_write(smote, paste0("ch3_forGAMs_prefire", x, "_smotesev", i, ".gpkg"), delete_dsn = T)
  }
}

thinfun(7)
thinfun(14)
thinfun(30)
thinfun(60)
thinfun(90)
thinfun(180)
