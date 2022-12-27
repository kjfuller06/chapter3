library(sf)
library(raster)
library(tidyverse)
library(exactextractr)
library(rgeos)

setwd("/glade/scratch/kjfuller/data")
# setwd("E:/chapter3/from Michael")
wind = st_read("wind_direction.gpkg")
# setwd("E:/chapter3/dwellings")
houses = read.csv("housing_density.csv") |> 
  dplyr::select(ID, house.density)

setwd("/glade/scratch/kjfuller/data/LFMC")
# setwd("E:/chapter3/from Rachael/R project/LFMC/LFMC/GEE_LFMC/LFMC_rasters/GEE_v20May2020_RevisedMask/")
lfmc = list.files()
lfmc = data.frame(files = lfmc)
lfmc$date = as.Date(substr(lfmc$files, 6, 15), format = "%Y_%m_%d")

setwd("/glade/scratch/kjfuller/data/VPD")
# setwd("E:/chapter3/from Rachael/VPD")
vpd = list.files()
vpd = data.frame(files = vpd)
vpd$date = as.Date(substr(vpd$files, 5, 12), format = "%Y%m%d")

setwd("/glade/scratch/kjfuller/data")
# setwd("D:/chapter1/bark-type-SDM/data")
fire_reg = read.csv("fire_regimes.csv") |> 
  dplyr::select(fueltype,
                fire_reg)

setwd("/glade/scratch/kjfuller/data/chapter3")
extractfun = function(x){
  tryCatch({
  # load isochrons ####
  setwd("/glade/scratch/kjfuller/data")
  # setwd("E:/chapter3/isochrons")
  g = st_read("isochrons_8days.gpkg")
  
  # LFMC ####
  setwd("/glade/scratch/kjfuller/data/LFMC")
  # setwd("E:/chapter3/from Rachael/R project/LFMC/LFMC/GEE_LFMC/LFMC_rasters/GEE_v20May2020_RevisedMask/")
  r = raster(lfmc$files[1])
  g = st_transform(g, crs = st_crs(r))
  ids = unique(g$ID)
  l = list()
  for(i in c(1:length(ids))){
    g_temp = g |>
      filter(ID == ids[i])
    
    ## select LFMC values that were calculated for dates before the fire started
    lfmc_temp = lfmc |>
      filter(as.numeric(difftime(min(g_temp$poly_sD), date)) > 0)
    ## select the last possible pre-fire LFMC value
    lfmc_temp = lfmc_temp |>
      filter(date == max(lfmc_temp$date))
    
    r = raster(lfmc_temp$files[1])
    
    g_temp$LFMC = raster::extract(r, g_temp, method = 'simple', fun = median)
    g_temp$LFMC_sD = lfmc_temp$date[1]
    
    if(any(is.na(g_temp$LFMC))){
      g_temp2 = g_temp |>
        filter(is.na(LFMC))
      if(g_temp2$LFMC_sD != min(lfmc$date)){
        lfmc_temp = lfmc |>
          filter(date < g_temp2$LFMC_sD[1])
        lfmc_temp = lfmc_temp |>
          filter(date == max(lfmc_temp$date))
        
        r = raster(lfmc_temp$files[1])
        
        g_temp2$LFMC = raster::extract(r, g_temp2, method = 'simple', fun = median)
        g_temp2$LFMC_sD = lfmc_temp$date[1]
        
        g_temp = g_temp |>
          filter(!is.na(LFMC)) |>
          rbind(g_temp2)
      }
    }
    l[[i]] = g_temp
    print(paste0("LFMC ", i))
  }
  g_temp = bind_rows(l)
  st_geometry(g_temp) = NULL
  g = full_join(g, g_temp)
  
  # VPD ####
  setwd("/glade/scratch/kjfuller/data/VPD")
  # setwd("E:/chapter3/from Rachael/VPD")
  r = raster(vpd$files[1])
  g = st_transform(g, crs = st_crs(4326))
  ids = unique(g$ID)
  l = list()
  for(i in c(1:length(ids))){
    g_temp = g |>
      filter(ID == ids[i])
    
    ## select VPD values that were calculated for dates after the fire started; for the fire polygon for each fire, extract values for the previous 24 hours
    vpd_temp = vpd |>
      filter(as.numeric(difftime(min(g_temp$poly_sD), date)) <= 0)
    vpd_temp = vpd_temp |>
      filter(as.numeric(difftime(min(g_temp$poly_eD), date)) >= 0)
    
    r = raster::stack(vpd_temp$files)
    
    g_temp$VPD = median(raster::extract(r, g_temp, method = 'simple', fun = median))
    g_temp$VPD_sD = vpd_temp$date[1]
    
    l[[i]] = g_temp
    print(paste0("VPD ", i))
  }
  g_temp = bind_rows(l)
  st_geometry(g_temp) = NULL
  g = full_join(g, g_temp)
  
  st_write(g, paste0("ch3_forGAMs_poly_prefire", x, "_dynamic.gpkg"), delete_dsn = T)
  # g = st_read(paste0("ch3_forGAMs_poly_prefire", x, "_dynamic.gpkg"))
  }, error = function(e){print(x); cat("ERROR :", conditionMessage(e), "\n")})
}

extractfun(7)
extractfun(14)
extractfun(30)
extractfun(60)
extractfun(90)
extractfun(180)

## still need to extract: fire type categories (FESM directly) -> not all isochrons align with FESM fires
## aspect, wind direction done, fire regime types (in order to restrict less representative veg types), number of dwellings, distance to roads and distance to water (take the min of both) done