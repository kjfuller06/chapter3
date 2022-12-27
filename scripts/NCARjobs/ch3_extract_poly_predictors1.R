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
  # load forest structure data ####
  # setwd("E:/chapter3/GEDI_FESM")
  gedi = st_read(paste0("ch3_isochrons_prefire", x, ".gpkg"))
  targetcrs = st_crs(gedi)
  st_geometry(gedi) = NULL
  gedi = gedi |> 
    dplyr::select(shot_number,
                  ID,
                  rh98,
                  cover_z_1,
                  over_cover,
                  fhd_normal)
  
  # load isochrons ####
  setwd("/glade/scratch/kjfuller/data")
  # setwd("E:/chapter3/isochrons")
  g = st_read("isochrons_8days.gpkg")
  
  # merge housing density, distance to breaks, and fire types ####
  g = g |> 
    left_join(houses) 
  
  # calculate and merge structural data ####
  g_agg = g |> 
    inner_join(gedi)
  # calculate the median of continuous variables
  g_agg1 = aggregate(data = g_agg, rh98 ~ ID, FUN = median)
  g_agg2 = aggregate(data = g_agg, cover_z_1 ~ ID, FUN = median)
  g_agg3 = aggregate(data = g_agg, over_cover ~ ID, FUN = median)
  g_agg4 = aggregate(data = g_agg, fhd_normal ~ ID, FUN = median)
  
  print("joining all median values of GEDI structure to isochron data")
  g = g |> 
    right_join(g_agg1) |> 
    right_join(g_agg2) |> 
    right_join(g_agg3) |> 
    right_join(g_agg4)
  
  # load rasters
  # setwd("D:/chapter1/other_data/Final/terrain variables")
  
  # elevation ####
  print("reading elevation file")
  r = raster("proj_dem_s.tif")
  
  print("converting to elevation crs")
  g = st_transform(g, crs = st_crs(r))
  print("extracting elevation")
  g$elevation = raster::extract(r, g, method = 'simple', fun = sd)
  
  print("filtering to exclude all NA values")
  g_na = g |> 
    filter(is.na(elevation))
  if(nrow(g_na) > 0){
    print("some NA's exist: extracting elevation at NA locations again with another method")
    elev_na = raster::extract(r, g_na, method = 'simple', exact = T)
    g_na$elevation = unlist(lapply(elev_na, sd))
    
    g = g |> 
      filter(!is.na(elevation))
    g = rbind(g, g_na)
  }
  
  # fire regimes ####
  r = raster("fuels_30m.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g$fueltype = raster::extract(r, g, method = 'simple', fun = median)
  g = g |> 
    left_join(fire_reg)
  
  # slope ####
  r = raster("proj_dem_slope_30m.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g$slope = raster::extract(r, g, method = 'simple', fun = median)
  
  # aspect ####
  r = raster("proj_dem_aspect_30m.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g$aspect = raster::extract(r, g, method = 'simple', fun = median)
  
  # distance to firelines ####
  r = raster("distancetofirelines_parallel.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g$firelines = raster::extract(r, g, method = 'simple', fun = median)
  
  # distance to roads ####
  r = raster("distancetoroads.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g$roads = raster::extract(r, g, method = 'simple', fun = median)
  
  # distance to water ####
  r = raster("distancetowater_relevance2.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g$water2 = raster::extract(r, g, method = 'simple', fun = median)
  
  r = raster("distancetowater_relevance3.tif")
  g$water3 = raster::extract(r, g, method = 'simple', fun = median)
  
  r = raster("distancetowater_relevance4.tif")
  g$water4 = raster::extract(r, g, method = 'simple', fun = median)
  
  # r = raster("distancetowater_relevance5.tif")
  # g$water5 = raster::extract(r, g, method = 'simple', fun = median)
  # 
  # r = raster("distancetowater_relevance6.tif")
  # g$water6 = raster::extract(r, g, method = 'simple', fun = median)
  
  # northness ####
  r = raster("proj_dem_northness_30m.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g$northness = raster::extract(r, g, method = 'simple', fun = median)
  
  # eastness ####
  r = raster("proj_dem_eastness_30m.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g$eastness = raster::extract(r, g, method = 'simple', fun = median)
  
  # bark types ####
  r = raster("NSW_stringybark_distribution.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g$stringybark = raster::extract(r, g, method = 'simple', fun = median)
  
  r = raster("NSW_ribboning_distribution.tif")
  g$ribbonbark = raster::extract(r, g, method = 'simple', fun = median)
  
  setwd("/glade/scratch/kjfuller/data/chapter3")
  g = st_transform(g, crs = targetcrs)
  st_write(g, paste0("ch3_forGAMs_poly_prefire", x, "_static.gpkg"), delete_dsn = T)
  # g = st_read(paste0("ch3_forGAMs_poly_prefire", x, "_static.gpkg"))
  
  print(paste0("*******************  ", x, " done  *********************"))
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