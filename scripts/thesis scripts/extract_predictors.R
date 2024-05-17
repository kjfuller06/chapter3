library(sf)
library(raster)
library(tidyverse)
library(exactextractr)
library(rgeos)
library(tmap)

# # GEDI ####
# setwd("E:/chapter3/isochrons")
# isochrons = st_read("isochrons_withfireline_f1.gpkg")
# length(unique(isochrons$ID))
# ## 70,370
# setwd("E:/chapter3/GEDI_FESM")
# # load data, create new polygons, intersecting the geometries of both
# gedi = st_read("ch3_FESMandfhist_forpoly.gpkg")
# targetcrs = st_crs(gedi)
# gedi = st_transform(gedi, st_crs(isochrons))
# g_buffer = st_buffer(gedi, dist = 12.5)
# g_buffer = st_intersection(g_buffer, isochrons)
# length(unique(g_buffer$ID))
# ## 5,061
# backup = g_buffer
# g_buffer$TUF = as.numeric(difftime(g_buffer$time, g_buffer$DateTime, units = "days"))
# g_buffer = g_buffer |>
#   filter(TUF > 0)
# length(unique(g_buffer$ID))
# ## 4,726
# 
# gedi = g_buffer |>
#   dplyr::select(ID, rh98:over_cover)
# st_geometry(gedi) = NULL
# 
# # calculate the median of continuous variables
# g_agg1 = aggregate(data = gedi, rh98 ~ ID, FUN = median)
# names(g_agg1)[2] = "rh98"
# g_agg1.1 = aggregate(data = gedi, rh98 ~ ID, FUN = function(x) quantile(x, probs = 0.1, na.rm = T))
# names(g_agg1.1)[2] = "rh98.1"
# g_agg1.9 = aggregate(data = gedi, rh98 ~ ID, FUN = function(x) quantile(x, probs = 0.9, na.rm = T))
# names(g_agg1.9)[2] = "rh98.9"
# g_agg2 = aggregate(data = gedi, cover ~ ID, FUN = median)
# names(g_agg2)[2] = "cover"
# g_agg2.1 = aggregate(data = gedi, cover ~ ID, FUN = function(x) quantile(x, probs = 0.1, na.rm = T))
# names(g_agg2.1)[2] = "cover.1"
# g_agg2.9 = aggregate(data = gedi, cover ~ ID, FUN = function(x) quantile(x, probs = 0.9, na.rm = T))
# names(g_agg2.9)[2] = "cover.9"
# g_agg3 = aggregate(data = gedi, cover_z_1 ~ ID, FUN = median)
# names(g_agg3)[2] = "cover_z_1"
# g_agg3.1 = aggregate(data = gedi, cover_z_1 ~ ID, FUN = function(x) quantile(x, probs = 0.1, na.rm = T))
# names(g_agg3.1)[2] = "cover_z_1.1"
# g_agg3.9 = aggregate(data = gedi, cover_z_1 ~ ID, FUN = function(x) quantile(x, probs = 0.9, na.rm = T))
# names(g_agg3.9)[2] = "cover_z_1.9"
# g_agg4 = aggregate(data = gedi, over_cover ~ ID, FUN = median)
# names(g_agg4)[2] = "over_cover"
# g_agg4.1 = aggregate(data = gedi, over_cover ~ ID, FUN = function(x) quantile(x, probs = 0.1, na.rm = T))
# names(g_agg4.1)[2] = "over_cover.1"
# g_agg4.9 = aggregate(data = gedi, over_cover ~ ID, FUN = function(x) quantile(x, probs = 0.9, na.rm = T))
# names(g_agg4.9)[2] = "over_cover.9"
# g_agg5 = aggregate(data = gedi, fhd_normal ~ ID, FUN = median)
# names(g_agg5)[2] = "fhd_normal"
# g_agg5.1 = aggregate(data = gedi, fhd_normal ~ ID, FUN = function(x) quantile(x, probs = 0.1, na.rm = T))
# names(g_agg5.1)[2] = "fhd_normal.1"
# g_agg5.9 = aggregate(data = gedi, fhd_normal ~ ID, FUN = function(x) quantile(x, probs = 0.9, na.rm = T))
# names(g_agg5.9)[2] = "fhd_normal.9"
# 
# g = gedi |>
#   dplyr::select(ID) |>
#   unique() |>
#   left_join(g_agg1) |>
#   left_join(g_agg1.1) |>
#   left_join(g_agg1.9) |>
#   left_join(g_agg2) |>
#   left_join(g_agg2.1) |>
#   left_join(g_agg2.9) |>
#   left_join(g_agg3) |>
#   left_join(g_agg3.1) |>
#   left_join(g_agg3.9) |>
#   left_join(g_agg4) |>
#   left_join(g_agg4.1) |>
#   left_join(g_agg4.9) |>
#   left_join(g_agg5) |>
#   left_join(g_agg5.1) |>
#   left_join(g_agg5.9)
# isochrons = left_join(isochrons, g, by = "ID")
# nrow(isochrons |> filter(is.na(rh98)))
# ## 65,644
# nrow(isochrons |> filter(!is.na(rh98)))
# ## 4,726
# 
# setwd("E:/chapter3/for GAMs")
# st_write(isochrons, "isochrons_gedi_m1.gpkg", delete_dsn = T)
# 
# terrain ####
setwd("D:/chapter1/data")
slope = raster("proj_dem_slope_30m.tif")
setwd("E:/chapter3")
rough = raster("terrain_roughness.tif")

setwd("E:/chapter3/for GAMs")
isochrons = st_read("isochrons_gedi_m1.gpkg")
targetcrs = st_crs(isochrons)
iso = isochrons |> 
  dplyr::select(ID)

iso = st_transform(iso, crs = st_crs(rough))
rough = raster::extract(rough, iso, method = 'simple')
iso$rough.1 = unlist(lapply(rough, FUN = function(x) quantile(x, probs = 0.1, na.rm = T)))
iso$rough.5 = unlist(lapply(rough, FUN = function(x) median(x, na.rm = T)))
iso$rough.9 = unlist(lapply(rough, FUN = function(x) quantile(x, probs = 0.9, na.rm = T)))

iso = st_transform(iso, crs = st_crs(slope))
slope = raster::extract(slope, iso, method = 'simple')
iso$slope.1 = unlist(lapply(slope, FUN = function(x) quantile(x, probs = 0.1, na.rm = T)))
iso$slope.5 = unlist(lapply(slope, FUN = function(x) median(x, na.rm = T)))
iso$slope.9 = unlist(lapply(slope, FUN = function(x) quantile(x, probs = 0.9, na.rm = T)))

st_geometry(iso) = NULL
isochrons = isochrons |> 
  left_join(iso)
st_write(isochrons, paste0("isochrons_terrain_m2.gpkg"), delete_dsn = T)

# fueltype ####
setwd("E:/chapter3/for GAMs")
isochrons = st_read("isochrons_terrain_m2.gpkg")
targetcrs = st_crs(isochrons)
g = isochrons |> 
  dplyr::select(ID)

setwd("D:/chapter1/data")
fire_reg = read.csv("fire_regimes.csv") |> 
  dplyr::select(fueltype,
                fire_reg)

r = raster("fuels_30m.tif")

g = st_transform(g, crs = st_crs(r))
fueltype = raster::extract(r, g, method = 'simple')

getmode = function(x){
  uniqv <- unique(unlist(x))
  uniqv = uniqv[!is.na(uniqv)]
  m = uniqv[which.max(tabulate(match(x, uniqv)))]
  return(m)
} 

isochrons$fueltype = unlist(lapply(fueltype, getmode))

setwd("E:/chapter3/for GAMs")
st_write(isochrons, "isochrons_fueltype_m3.gpkg", delete_dsn = T)

# barktype ####
setwd("E:/chapter3/for GAMs")
iso = st_read("isochrons_fueltype_m3.gpkg")
targetcrs = st_crs(iso)
g = iso |>
  dplyr::select(ID)

setwd("D:/chapter1/Major outputs/final maps/final maps")
r = raster("NSW_stringybark_distribution_final.tif")

g = st_transform(g, crs = st_crs(r))
l = raster::extract(r, g, method = 'simple')
g$stringybark.1 = unlist(lapply(l, FUN = function(x) quantile(x, probs = 0.1, na.rm = T)))
g$stringybark.5 = unlist(lapply(l, FUN = function(x) median(x, na.rm = T)))
g$stringybark.9 = unlist(lapply(l, FUN = function(x) quantile(x, probs = 0.9, na.rm = T)))

r = raster("NSW_ribbonbark_distribution_final.tif")

g = st_transform(g, crs = st_crs(r))
l = raster::extract(r, g, method = 'simple')
g$ribbonbark.1 = unlist(lapply(l, FUN = function(x) quantile(x, probs = 0.1, na.rm = T)))
g$ribbonbark.5 = unlist(lapply(l, FUN = function(x) median(x, na.rm = T)))
g$ribbonbark.9 = unlist(lapply(l, FUN = function(x) quantile(x, probs = 0.9, na.rm = T)))

st_geometry(g) = NULL
iso = iso |> 
  left_join(g, by = "ID")

setwd("E:/chapter3/for GAMs")
st_write(iso, "isochrons_barktype_m4.gpkg", delete_dsn = T)

# LFMC ####
setwd("E:/chapter3/original/from Rachael/R project/LFMC/LFMC/GEE_LFMC/LFMC_rasters/GEE_v20May2020_RevisedMask/")
lfmc = list.files(pattern = "LFMC")
lfmc = data.frame(files = lfmc)
lfmc$date = as.POSIXct(substr(lfmc$files, 6, 15), format = "%Y_%m_%d")
## using as.Date led to problems for this extraction but the severity data extraction did fine- double-checked

setwd("E:/chapter3/for GAMs")
iso = st_read("isochrons_barktype_m4.gpkg")
targetcrs = st_crs(iso)
iso$poly_sD = as.POSIXct(iso$lasttim)
iso$poly_eD = as.POSIXct(iso$time)
g = iso |>
  dplyr::select(ID,
                poly_sD,
                poly_eD)

setwd("E:/chapter3/original/from Rachael/R project/LFMC/LFMC/GEE_LFMC/LFMC_rasters/GEE_v20May2020_RevisedMask/")
r = raster(lfmc$files[1])
g = st_transform(g, crs = st_crs(r))
ids = unique(g$ID)

counter <- 0
l <- list(NULL)
size <- 1
dynamicfun = function(x){
  if( .GlobalEnv$counter == .GlobalEnv$size )
  {length(.GlobalEnv$l) <- .GlobalEnv$size <- .GlobalEnv$size * 2}
  
  g_temp = g |>
    filter(ID == ids[x])
  
  ## select LFMC values that were calculated for dates before the fire started
  lfmc_temp = lfmc |>
    filter(as.numeric(difftime(min(g_temp$poly_sD), date)) > 0)
  ## select the last possible pre-fire LFMC value
  minT = lfmc_temp |>
    filter(date == max(lfmc_temp$date))
  
  ## select LFMC values that were calculated for dates before the fire end
  lfmc_temp = lfmc |>
    filter(as.numeric(difftime(max(g_temp$poly_eD), date)) > 0)
  ## select the last possible during-fire LFMC value
  maxT = lfmc_temp |>
    filter(date == max(lfmc_temp$date))
  
  lfmc_temp = lfmc |>
    filter(date >= minT$date & date <= maxT$date)
  
  l = list()
  if(nrow(lfmc_temp) > 1){
    print(paste0("number of rasters = ", nrow(lfmc_temp)))
    r_temp = raster(lfmc_temp$files[nrow(lfmc_temp)])
    for(v in c(nrow(lfmc_temp):2)){
      r_mask = raster(lfmc_temp$files[v-1])
      r_temp = overlay(r_temp, r_mask, fun = function(x, y) {x[is.na(x)] <- y[is.na(x)]; x})
    }
    l = raster::extract(r_temp, g_temp, method = 'simple')
    g_temp$LFMC = median(unlist(l), na.rm = T)
    g_temp$LFMC.1 = quantile(unlist(l), probs = 0.1, na.rm = T)
    g_temp$LFMC.9 = quantile(unlist(l), probs = 0.9, na.rm = T)
  } else {
    print("only 1 LFMC raster")
    r = raster(lfmc_temp$files[1])
    l = raster::extract(r, g_temp, method = 'simple')
    g_temp$LFMC = median(unlist(l), na.rm = T)
    g_temp$LFMC.1 = quantile(unlist(l), probs = 0.1, na.rm = T)
    g_temp$LFMC.9 = quantile(unlist(l), probs = 0.9, na.rm = T)
  }
  g_temp$LFMC_sD = lfmc_temp$date[1]
  
  if(any(is.na(g_temp$LFMC))){
    g_temp2 = g_temp |>
      filter(is.na(LFMC))
    if(g_temp2$LFMC_sD != min(lfmc$date)){
      print("LFMC values were NA, attempting to extract earlier LFMC values")
      lfmc_temp = lfmc |>
        filter(date < g_temp2$LFMC_sD[1])
      lfmc_temp = lfmc_temp |>
        filter(date == max(lfmc_temp$date))
      
      r = raster(lfmc_temp$files[1])
      
      l = raster::extract(r, g_temp2, method = 'simple')
      g_temp2$LFMC = median(unlist(l), na.rm = T)
      g_temp2$LFMC.1 = quantile(unlist(l), probs = 0.1, na.rm = T)
      g_temp2$LFMC.9 = quantile(unlist(l), probs = 0.9, na.rm = T)
      
      g_temp2$LFMC_sD = lfmc_temp$date[1]
      
      if(any(!is.na(g_temp2$LFMC))){
        print("Earlier LFMC value extracted, at least some values not NA")
        g_temp = g_temp |>
          filter(!is.na(LFMC)) |>
          rbind(g_temp2)
      } else {
        print("Earlier LFMC values were also NA")
      }
    }
  }
  # l[[i]] = g_temp
  
  st_geometry(g_temp) = NULL
  
  .GlobalEnv$counter <- .GlobalEnv$counter + 1
  .GlobalEnv$l[[.GlobalEnv$counter]] <- g_temp
}
for(i in c(1:length(ids))){
  dynamicfun(i)
  print(paste0("LFMC ", i))
}

g_temp = bind_rows(l)
g_temp = g_temp |> 
  dplyr::select(-c(poly_sD, poly_eD))
iso = iso |> 
  left_join(g_temp, by = "ID")

setwd("E:/chapter3/for GAMs")
st_write(iso, "isochrons_LFMC_m5.gpkg", delete_dsn = T)

# VPD ####
setwd("E:/chapter3/original/from Rachael/VPD")
vpd = list.files()
vpd = data.frame(files = vpd)
vpd$date = as.POSIXct(substr(vpd$files, 5, 12), format = "%Y%m%d")

setwd("E:/chapter3/for GAMs")
iso = st_read("isochrons_LFMC_m5.gpkg")
targetcrs = st_crs(iso)
g = iso |> 
  dplyr::select(ID,
                poly_sD,
                poly_eD)

setwd("E:/chapter3/original/from Rachael/VPD")
r = raster(vpd$files[1])
crs(r) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
## res 0.05
g = st_transform(g, crs = st_crs(CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")))
ids = unique(g$ID)

counter <- 0
l <- list(NULL)
size <- 1
dynamicfun = function(x){
  if( .GlobalEnv$counter == .GlobalEnv$size )
  {length(.GlobalEnv$l) <- .GlobalEnv$size <- .GlobalEnv$size * 2}
  g_temp = g |>
    filter(ID == ids[i])
  
  ## select VPD values that were calculated for dates before the fire started
  vpd_temp = vpd |>
    filter(as.numeric(difftime(min(g_temp$poly_sD), date)) >= 0)
  ## select the last possible pre-fire VPD value
  minT = vpd_temp |>
    filter(date == max(vpd_temp$date))
  
  ## select VPD values that were calculated for dates before the fire end
  vpd_temp = vpd |>
    filter(as.numeric(difftime(max(g_temp$poly_eD), date)) >= 0)
  ## select the last possible during-fire VPD value
  maxT = vpd_temp |>
    filter(date == max(vpd_temp$date))
  
  vpd_temp = vpd |>
    filter(date >= minT$date & date <= maxT$date)
  
  l = list()
  if(nrow(vpd_temp) > 0){
    for(v in c(1:nrow(vpd_temp))){
      r_temp = raster(vpd_temp$files[v])
      crs(r_temp) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
      l[[v]] = raster::extract(r_temp, g_temp, method = 'simple')
    }
    g_temp$VPD = median(unlist(l), na.rm = T)
    g_temp$VPD.1 = quantile(unlist(l), probs = 0.1, na.rm = T)
    g_temp$VPD.9 = quantile(unlist(l), probs = 0.9, na.rm = T)
  } else {
    r = raster(vpd_temp$files[1])
    crs(r) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
    l = raster::extract(r, g_temp, method = 'simple')
    g_temp$VPD = median(unlist(l), na.rm = T)
    g_temp$VPD.1 = quantile(unlist(l), probs = 0.1, na.rm = T)
    g_temp$VPD.9 = quantile(unlist(l), probs = 0.9, na.rm = T)
  }
  g_temp$VPD_sD = vpd_temp$date[1]
  
  # l[[i]] = g_temp
  
  st_geometry(g_temp) = NULL
  
  .GlobalEnv$counter <- .GlobalEnv$counter + 1
  .GlobalEnv$l[[.GlobalEnv$counter]] <- g_temp
}

for(i in c(1:length(ids))){
  dynamicfun(i)
  print(paste0("VPD ", i))
}

g_temp = bind_rows(l)
g_temp = g_temp |> 
  dplyr::select(-c(poly_sD, poly_eD))
st_geometry(g_temp)
iso = iso |> 
  left_join(g_temp, by = "ID")

setwd("E:/chapter3/for GAMs")
st_write(iso, "isochrons_VPD_m6.gpkg", delete_dsn = T)

# wind ####
setwd("E:/chapter3/for GAMs")
iso = st_read("isochrons_VPD_m6.gpkg")
targetcrs = st_crs(iso)
g = iso |>
  dplyr::select(ID,
                poly_sD,
                poly_eD)

index = g |>
  dplyr::select(poly_sD,
                poly_eD)
st_geometry(index) = NULL
index = unique(index)
index$index = c(1:nrow(index))
g = g |>
  left_join(index)

g_buffer = st_buffer(g, dist = 100000)

setwd("E:/chapter3/from Michael")
wind = st_read("wind_direction.gpkg")
wind = st_transform(wind, crs = st_crs(g))

stations = wind |>
  dplyr::select(station)
stations = stations[!duplicated(stations$station),]
st_geometry(wind) = NULL

counter <- 0
l <- list(NULL)
size <- 1
windfun = function(x){
  if( .GlobalEnv$counter == .GlobalEnv$size )
  {length(.GlobalEnv$l) <- .GlobalEnv$size <- .GlobalEnv$size * 2}
  
  tryCatch({
    # select a buffered polygon and index all stations within 100km
    g_temp = g_buffer[g_buffer$index == unique(g_buffer$index)[x],]
    stat_temp = stations[g_temp,]
    
    # select the original polygon and calculate distance from centroid to selected stations
    g_temp = g |>
      filter(ID %in% g_temp$ID)
    g_center = st_centroid(g_temp)
    dist = as.data.frame(st_distance(g_temp, stat_temp))
    st_geometry(stat_temp) = NULL
    st_geometry(g_temp) = NULL
    stat_temp = cbind(stat_temp, t(dist))
    
    # select wind data based on polygon timeframe
    wind_temp = wind |>
      filter(station %in% stat_temp$station) |>
      left_join(stat_temp)
    minT = wind_temp |>
      filter(DateTime <= min(g_temp$poly_sD))
    minT = max(minT$DateTime, na.rm = T)
    maxT = wind_temp |>
      filter(DateTime >= max(g_temp$poly_eD))
    maxT = min(maxT$DateTime, na.rm = T)
    wind_temp = wind_temp |>
      filter(DateTime >= minT) |>
      filter(DateTime <= maxT)
    
    # calculate the median wind speed within the polygon timeframe at each selected station, join to station data
    windsp1 = aggregate(data = wind_temp, windspeed ~ station, FUN = function(x) sd(x, na.rm = T))
    names(windsp1)[2] = "windspeed.stdev"
    windsp2 = aggregate(data = wind_temp, windspeed ~ station, FUN = function(x) median(x, na.rm = T))
    names(windsp2)[2] = "windspeed"
    windsp3 = aggregate(data = wind_temp, windspeed ~ station, FUN = function(x) quantile(x, probs = 0.1, na.rm = T))
    names(windsp3)[2] = "windspeed.1"
    windsp4 = aggregate(data = wind_temp, windspeed ~ station, FUN = function(x) quantile(x, probs = 0.9, na.rm = T))
    names(windsp4)[2] = "windspeed.9"
    
    windgt1 = aggregate(data = wind_temp, windgust ~ station, FUN = function(x) sd(x, na.rm = T))
    names(windgt1)[2] = "windgust.stdev"
    windgt2 = aggregate(data = wind_temp, windgust ~ station, FUN = function(x) median(x, na.rm = T))
    names(windgt2)[2] = "windgust"
    windgt3 = aggregate(data = wind_temp, windgust ~ station, FUN = function(x) quantile(x, probs = 0.9, na.rm = T))
    names(windgt3)[2] = "windgust.9"
    
    winddir1 = aggregate(data = wind_temp, winddir ~ station, FUN = function(x) median(x[x != 0], na.rm = T))
    names(winddir1)[2] = "winddir"
    winddir2 = aggregate(data = wind_temp, winddir ~ station, FUN = function(x) sd(x[x != 0], na.rm = T))
    names(winddir2)[2] = "winddir.stdev"
    
    wind_temp = stat_temp |>
      full_join(windsp1) |>
      full_join(windsp2) |>
      full_join(windsp3) |>
      full_join(windsp4) |>
      full_join(windgt1) |>
      full_join(windgt2) |>
      full_join(windgt3) |>
      full_join(winddir1) |>
      full_join(winddir2)
    
    weights = wind_temp |> dplyr::select(c(2:(ncol(wind_temp) - 9)))
    weights[weights > 100000] = 100000
    
    # create distance-based weights and calculate distance-weighted mean and wind impact index
    weights = as.data.frame(lapply(weights, function(x) {1 - (x/100000)}))
    g_temp$windspeed.stdev = as.numeric(lapply(weights,
                                               function(x) {weighted.mean(wind_temp[,"windspeed.stdev"], x, na.rm = T)}))
    g_temp$windspeed = as.numeric(lapply(weights,
                                         function(x) {weighted.mean(wind_temp[,"windspeed"], x, na.rm = T)}))
    g_temp$windspeed.1 = as.numeric(lapply(weights,
                                           function(x) {weighted.mean(wind_temp[,"windspeed.1"], x, na.rm = T)}))
    g_temp$windspeed.9 = as.numeric(lapply(weights,
                                           function(x) {weighted.mean(wind_temp[,"windspeed.9"], x, na.rm = T)}))
    g_temp$windgust.stdev = as.numeric(lapply(weights,
                                              function(x) {weighted.mean(wind_temp[,"windgust.stdev"], x, na.rm = T)}))
    g_temp$windgust = as.numeric(lapply(weights,
                                        function(x) {weighted.mean(wind_temp[,"windgust"], x, na.rm = T)}))
    g_temp$windgust.9 = as.numeric(lapply(weights,
                                          function(x) {weighted.mean(wind_temp[,"windgust.9"], x, na.rm = T)}))
    g_temp$winddir.stdev = as.numeric(lapply(weights,
                                             function(x) {weighted.mean(wind_temp[,"winddir.stdev"], x, na.rm = T)}))
    g_temp$winddir = as.numeric(lapply(weights,
                                       function(x) {weighted.mean(wind_temp[,"winddir"], x, na.rm = T)}))
    
    .GlobalEnv$counter <- .GlobalEnv$counter + 1
    .GlobalEnv$l[[.GlobalEnv$counter]] <- g_temp
    
  }, error = function(e){print(x); cat("ERROR :", conditionMessage(e), "\n")})
}
for(i in c(1:length(unique(g_buffer$index)))){
  windfun(i)
  print(i)
}
g_temp = bind_rows(l)
setwd("E:/chapter3/for GAMs")
write.csv(g_temp, "wind_m7.csv", row.names = F)

g_temp = g_temp |> 
  dplyr::select(-c(poly_sD, poly_eD, index))
iso = iso |> 
  left_join(g_temp, by = "ID")
st_write(iso, "isochrons_wind_m7.gpkg", delete_dsn = T)

# # strahler ####
# setwd("E:/chapter3/for GAMs")
# # iso = st_read("isochrons_withfireline_f1.gpkg")
# iso = st_read("isochrons_wind_m7.gpkg")
# targetcrs = st_crs(iso)
# g = iso |> 
#   dplyr::select(ID)
# 
# setwd("D:/chapter3/climatedem")
# g.temp = list()
# for(i in c(12:3)){
#   i.temp = as.character(c(i:12))
#   s_all = list()
#   for(x in i.temp){
#     s.temp = st_read(paste0("strahler", x, ".gpkg")) |> st_transform(crs = targetcrs)
#     names(s.temp)[1:2] = c("cat", "value")
#     s.temp$value = as.numeric(s.temp$value)
#     s_all[[x]] = s.temp
#   }
#   s_all = bind_rows(s_all)
#   
#   s.temp = st_intersection(s_all, g)
#   if(nrow(s.temp) > 0){
#     s.temp$length = as.numeric(st_length(s.temp))
#     st_geometry(s.temp) = NULL
#     s.temp = aggregate(data = s.temp, length ~ ID, FUN = sum)
#     s.temp$length[is.na(s.temp$length)] = 0
#     names(s.temp)[2] = paste0("strahler", i)
#     
#     g.temp[[i]] = g |> 
#       left_join(s.temp, by = "ID")
#   }
# }
# g = bind_cols(g.temp)
# g = g |> 
#   dplyr::select(c(1, 2, 5, 8, 11, 14, 17, 20, 23))
# names(g)[1] = "ID"
# st_geometry(g) = NULL
# iso = iso |> 
#   left_join(g, by = "ID")
# st_write(iso, paste0("isochrons_strahler_m8.gpkg"), delete_dsn = T)
# ## many NaNs- should they be 0?
# 
# look at NaNs ####
tmap_mode("view")
setwd("E:/chapter3/for GAMs")
iso = st_read("isochrons_strahler_m8.gpkg")
iso
iso = st_make_valid(iso)
nrow(iso)
## 70,370

setwd("D:/chapter1/data")
nsw = st_read("NSW_sans_islands.shp") |> 
  st_transform(crs = st_crs(iso))

iso.yes = st_contains(nsw, iso)
iso = iso[unlist(iso.yes),]
nrow(iso)
## 70,076

# - - - wind - - - #
nrow(iso |> filter(is.na(winddir)))
## 270
tm_shape(iso |> filter(is.na(winddir))) + tm_polygons()
iso |> filter(is.na(winddir))
## all wind variables are NaN
iso = iso |> 
  filter(!is.na(winddir))
nrow(iso)
## 69,806

nrow(iso |> filter(is.na(windgust.stdev)))
## 390
tm_shape(iso |> filter(is.na(windgust.stdev))) + tm_polygons()
iso |> filter(is.na(windgust.stdev))
## few other variables NaN
iso |> filter(is.na(windgust))
## only two observations, worth removing
iso = iso |> 
  filter(!is.na(windgust))
nrow(iso)
## 69,804

# - - - LFMC - - - #
nrow(iso |> filter(is.na(LFMC)))
## 3,568
tm_shape(iso |> filter(is.na(LFMC))) + tm_polygons()
iso |> filter(is.na(LFMC))
## LFMC is a critical variable
iso = iso |> 
  filter(!is.na(LFMC))
nrow(iso)
## 66,236

# - - - barktypes - - - #
nrow(iso |> filter(is.na(stringybark.1)))
## 5,017
tm_shape(iso |> filter(is.na(stringybark.1))) + tm_polygons()
iso |> filter(is.na(stringybark.1))
summary(iso |> filter(is.na(stringybark.1)))
sort(unique(iso$fueltype[is.na(iso$stringybark.1)]))
iso.b = iso |> 
  filter(is.na(stringybark.1))
g = iso.b |>
  dplyr::select(ID)

setwd("D:/chapter1/Major outputs/final maps/final maps")
r = raster("NSW_stringybark_distribution_final.tif")

g = st_transform(g, crs = st_crs(r))
l = raster::extract(r, g, method = 'simple')
summary(unlist(lapply(l, FUN = function(x) quantile(x, probs = 0.1, na.rm = T))))
## all extracted values are NaN
iso = iso |> 
  filter(!is.na(stringybark.1))
nrow(iso)
## 61,219

# - - - Strahler - - - #
nrow(iso |> filter(is.na(strahler4)))
## 33,708
nrow(iso |> filter(is.na(strahler11)))
## 61,137
min(iso$strahler4, na.rm = T)
## all NaNs are length of stream = 0
iso |> filter(is.na(strahler4) & !is.na(strahler11))
iso |> filter(is.na(strahler11) & !is.na(strahler4))
## lengths of stream added to each successive Strahler classification (larger stream lengths added to smaller streams, not the other way around)
## NaNs caused by no streams being present

iso$strahler4[is.na(iso$strahler4)] = 0
iso$strahler5[is.na(iso$strahler5)] = 0
iso$strahler6[is.na(iso$strahler6)] = 0
iso$strahler7[is.na(iso$strahler7)] = 0
iso$strahler8[is.na(iso$strahler8)] = 0
iso$strahler9[is.na(iso$strahler9)] = 0
iso$strahler10[is.na(iso$strahler10)] = 0
iso$strahler11[is.na(iso$strahler11)] = 0
summary(iso)

# - - - GEDI - - - #
nrow(iso |> filter(is.na(rh98)))
## 56,658
## this is the VAST majority of fire polygons!!

setwd("E:/chapter3/for GAMs")
st_write(iso, "isochrons_prep1.gpkg", delete_dsn = T)

# GEDI, redo ####
setwd("E:/chapter3/for GAMs")
isochrons = st_read("isochrons_prep1.gpkg")
length(unique(isochrons$ID))
## 61,219
isochrons = isochrons |> 
  dplyr::select(-c(rh98:fhd_normal.9))

setwd("E:/chapter3/GEDI_FESM")
# load data, create new polygons, intersecting the geometries of both
gedi = st_read("ch3_FESMandfhist_forpoly.gpkg")
targetcrs = st_crs(gedi)
gedi = st_transform(gedi, st_crs(isochrons))
nrow(gedi)
## 110,897

gedi$TUF = as.numeric(difftime(max(isochrons$time), gedi$DateTime, units = "days"))
gedi = gedi |>
  filter(TUF > 0)
nrow(gedi)
## 108,172

# overlapping GEDI and isochrons
g.temp = st_intersection(gedi, isochrons)
g_agg1 = aggregate(data = g.temp, rh98 ~ ID, FUN = median)
names(g_agg1)[2] = "rh98"
g_agg2 = aggregate(data = g.temp, cover ~ ID, FUN = median)
names(g_agg2)[2] = "cover"
g_agg3 = aggregate(data = g.temp, cover_z_1 ~ ID, FUN = median)
names(g_agg3)[2] = "cover_z_1"
g_agg4 = aggregate(data = g.temp, over_cover ~ ID, FUN = median)
names(g_agg4)[2] = "over_cover"
g_agg5 = aggregate(data = g.temp, fhd_normal ~ ID, FUN = median)
names(g_agg5)[2] = "fhd_normal"

g.temp = g.temp |>
  dplyr::select(ID)
st_geometry(g.temp) = NULL
g.temp = g.temp |>
  unique() |>
  left_join(g_agg1) |>
  left_join(g_agg2) |>
  left_join(g_agg3) |>
  left_join(g_agg4) |>
  left_join(g_agg5)
g.temp = inner_join(isochrons, g.temp)
g.temp$Gmethod = "median"

# non-overlapping GEDI and isochrons
iso.temp = isochrons |> 
  filter(!ID %in% g.temp$ID)
g.temp.dist = st_nearest_feature(iso.temp, g.temp)
iso.temp$shot_number = gedi$shot_number[g.temp.dist]

st_geometry(gedi) = NULL
iso.temp = iso.temp |> 
  left_join(gedi)
iso.temp = iso.temp |> 
  dplyr::select(c(time:strahler11, rh98:over_cover))
iso.temp$Gmethod = "nearest"

iso.temp = rbind(g.temp, iso.temp)
setwd("E:/chapter3/for GAMs")
st_write(iso.temp, "isochrons_prep2.gpkg", delete_dsn = T)

# prep ####
setwd("D:/chapter1/data")
fireregs = read.csv("fire_regimes.csv")

setwd("E:/chapter3/for GAMs")
isochrons = st_read("isochrons_prep2.gpkg")
length(unique(isochrons$ID))
## 61,219

isochrons = isochrons |> filter(fueltype < 42)
isochrons = isochrons |> left_join(fireregs)
nrow(isochrons)
## 58,888

isochrons$fire_reg[isochrons$fire_reg == 7] = 6

isochrons$logprog = log(isochrons$prog)
isochrons$logspread = log(isochrons$spread)

isochrons$stringybark.5[isochrons$stringybark.5 == 0.5] = 1
isochrons$stringybark.5 = as.factor(isochrons$stringybark.5)

isochrons$ribbonbark.5[isochrons$ribbonbark.5 == 0.5] = 1
isochrons$ribbonbark.5 = as.factor(isochrons$ribbonbark.5)

isochrons$s11 = isochrons$strahler11/isochrons$area
isochrons$s10 = isochrons$strahler10/isochrons$area
isochrons$s9 = isochrons$strahler9/isochrons$area
isochrons$s8 = isochrons$strahler8/isochrons$area
isochrons$s7 = isochrons$strahler7/isochrons$area
isochrons$s6 = isochrons$strahler6/isochrons$area
isochrons$s5 = isochrons$strahler5/isochrons$area
isochrons$s4 = isochrons$strahler4/isochrons$area

isochrons$windnorth = cos(isochrons$winddir * pi / 180)
isochrons$windeast = sin(isochrons$winddir * pi / 180)

isochrons$ffdi_cat = NA
isochrons$ffdi_cat[isochrons$ffdi_final <= 12] = "one"
isochrons$ffdi_cat[isochrons$ffdi_final > 12 & isochrons$ffdi_final <= 25] = "two"
isochrons$ffdi_cat[isochrons$ffdi_final > 25 & isochrons$ffdi_final <= 49] = "three"
isochrons$ffdi_cat[isochrons$ffdi_final > 49 & isochrons$ffdi_final <= 74] = "four"
isochrons$ffdi_cat[isochrons$ffdi_final > 74 & isochrons$ffdi_final <= 99] = "four"
isochrons$ffdi_cat[isochrons$ffdi_final > 99] = "four"
isochrons$ffdi_cat = factor(isochrons$ffdi_cat, levels = c("one",
                                           "two",
                                           "three",
                                           "four"))

isochrons = na.omit(isochrons)
nrow(isochrons)
##58,556

b = isochrons
st_geometry(b) = NULL
b |> group_by(fire_reg) |> tally() |> as.data.frame()
# rainforest:                 3378
# wet sclerophyll (shrubby):  9619
# wet sclerophyll (grassy):   9635
# dry sclerophyll (s/g):      11506
# dry sclerophyll (shrubby):  23397
# grassy woodland:            1348

st_write(isochrons, "isochrons_prep3.gpkg", delete_dsn = T)

# strahler, redo ####
setwd("E:/chapter3/for GAMs")
iso = st_read("isochrons_prep3.gpkg")
targetcrs = st_crs(iso)
g = iso |> 
  dplyr::select(ID)

setwd("D:/chapter3/climatedem")
g.temp = list()
for(i in c(11:4)){
  s_all = st_read(paste0("strahler", i, ".gpkg")) |> st_transform(crs = targetcrs)
  names(s_all)[1:2] = c("cat", "value")
  s_all$value = as.numeric(s_all$value)
  
  s.temp = st_intersection(s_all, g)
  if(nrow(s.temp) > 0){
    s.temp$length = as.numeric(st_length(s.temp))
    st_geometry(s.temp) = NULL
    s.temp = aggregate(data = s.temp, length ~ ID, FUN = sum)
    s.temp$length[is.na(s.temp$length)] = 0
    names(s.temp)[2] = paste0("strahler", i, ".only")
    
    g.temp[[i]] = s.temp
  }
}
g = iso |> 
  left_join(g.temp[[4]]) |> 
  left_join(g.temp[[5]]) |> 
  left_join(g.temp[[6]]) |> 
  left_join(g.temp[[7]]) |> 
  left_join(g.temp[[8]]) |> 
  left_join(g.temp[[9]]) |> 
  left_join(g.temp[[10]]) |> 
  left_join(g.temp[[11]])

g$strahler4.only[is.na(g$strahler4.only)] = 0
g$strahler5.only[is.na(g$strahler5.only)] = 0
g$strahler6.only[is.na(g$strahler6.only)] = 0
g$strahler7.only[is.na(g$strahler7.only)] = 0
g$strahler8.only[is.na(g$strahler8.only)] = 0
g$strahler9.only[is.na(g$strahler9.only)] = 0
g$strahler10.only[is.na(g$strahler10.only)] = 0
g$strahler11.only[is.na(g$strahler11.only)] = 0

g$s11.only = g$strahler11.only/g$area
g$s10.only = g$strahler10.only/g$area
g$s9.only = g$strahler9.only/g$area
g$s8.only = g$strahler8.only/g$area
g$s7.only = g$strahler7.only/g$area
g$s6.only = g$strahler6.only/g$area
g$s5.only = g$strahler5.only/g$area
g$s4.only = g$strahler4.only/g$area

setwd("E:/chapter3/for GAMs")
st_write(g, "isochrons_prep4.gpkg", delete_dsn = T)

# GEDI, redo, redo ####
setwd("E:/chapter3/for GAMs")
isochrons = st_read("isochrons_prep4.gpkg")
length(unique(isochrons$ID))
## 58,556
names(isochrons)[names(isochrons) == "rh98" | names(isochrons) == "cover" | names(isochrons) == "cover_z_1" | names(isochrons) == "over_cover" | names(isochrons) == "fhd_normal" | names(isochrons) == "Gmethod"] = c("rh98.all", "cover.all", "cover_z_1.all", "over_cover.all", "fhd_normal.all", "Gmethod.all")

setwd("E:/chapter3/GEDI_FESM")
gedi = st_read("ch3_FESMandfhist_forpoly.gpkg")
targetcrs = st_crs(gedi)
gedi = st_transform(gedi, st_crs(isochrons))
nrow(gedi)
## 110,897

gedi$TUF = as.numeric(difftime(max(isochrons$time), gedi$DateTime, units = "days"))
gedi = gedi |>
  filter(TUF > 0)
nrow(gedi)
## 108,172

# 12.5 m
g_buffer = st_buffer(gedi, dist = 12.5)

# overlapping GEDI and isochrons
g.temp = st_intersection(g_buffer, isochrons)
g_agg1 = aggregate(data = g.temp, rh98 ~ ID, FUN = median)
names(g_agg1)[2] = "rh98.12.5"
g_agg2 = aggregate(data = g.temp, cover ~ ID, FUN = median)
names(g_agg2)[2] = "cover.12.5"
g_agg3 = aggregate(data = g.temp, cover_z_1 ~ ID, FUN = median)
names(g_agg3)[2] = "cover_z_1.12.5"
g_agg4 = aggregate(data = g.temp, over_cover ~ ID, FUN = median)
names(g_agg4)[2] = "over_cover.12.5"
g_agg5 = aggregate(data = g.temp, fhd_normal ~ ID, FUN = median)
names(g_agg5)[2] = "fhd_normal.12.5"

g.temp = g.temp |>
  dplyr::select(ID)
st_geometry(g.temp) = NULL
g.temp.12.5 = g.temp |>
  unique() |>
  left_join(g_agg1) |>
  left_join(g_agg2) |>
  left_join(g_agg3) |>
  left_join(g_agg4) |>
  left_join(g_agg5)
nrow(g.temp.12.5)
## 4,287
g.temp = isochrons |> 
  dplyr::select(ID)
g.temp = full_join(g.temp, g.temp.12.5)
setwd("E:/chapter3/for GAMs")
st_write(g.temp, "isochrons_GEDI12.5.gpkg", delete_dsn = T)
rm(g_agg1, g_agg2, g_agg3, g_agg4, g_agg5, g.temp.12.5, g.temp)

# 25 m
g_buffer = st_buffer(gedi, dist = 25)

# overlapping GEDI and isochrons
g.temp = st_intersection(g_buffer, isochrons)
g_agg1 = aggregate(data = g.temp, rh98 ~ ID, FUN = median)
names(g_agg1)[2] = "rh98.25"
g_agg2 = aggregate(data = g.temp, cover ~ ID, FUN = median)
names(g_agg2)[2] = "cover.25"
g_agg3 = aggregate(data = g.temp, cover_z_1 ~ ID, FUN = median)
names(g_agg3)[2] = "cover_z_1.25"
g_agg4 = aggregate(data = g.temp, over_cover ~ ID, FUN = median)
names(g_agg4)[2] = "over_cover.25"
g_agg5 = aggregate(data = g.temp, fhd_normal ~ ID, FUN = median)
names(g_agg5)[2] = "fhd_normal.25"

g.temp = g.temp |>
  dplyr::select(ID)
st_geometry(g.temp) = NULL
g.temp.25 = g.temp |>
  unique() |>
  left_join(g_agg1) |>
  left_join(g_agg2) |>
  left_join(g_agg3) |>
  left_join(g_agg4) |>
  left_join(g_agg5)
nrow(g.temp.25)
## 4,626, an increase of 339 shots over buffer of 12.5 m
g.temp = isochrons |> 
  dplyr::select(ID)
g.temp = full_join(g.temp, g.temp.25)
setwd("E:/chapter3/for GAMs")
st_write(g.temp, "isochrons_GEDI25.gpkg", delete_dsn = T)
rm(g_agg1, g_agg2, g_agg3, g_agg4, g_agg5, g.temp.25, g.temp)

# 100 m
g_buffer = st_buffer(gedi, dist = 100)

# overlapping GEDI and isochrons
g.temp = st_intersection(g_buffer, isochrons)
g_agg1 = aggregate(data = g.temp, rh98 ~ ID, FUN = median)
names(g_agg1)[2] = "rh98.100"
g_agg2 = aggregate(data = g.temp, cover ~ ID, FUN = median)
names(g_agg2)[2] = "cover.100"
g_agg3 = aggregate(data = g.temp, cover_z_1 ~ ID, FUN = median)
names(g_agg3)[2] = "cover_z_1.100"
g_agg4 = aggregate(data = g.temp, over_cover ~ ID, FUN = median)
names(g_agg4)[2] = "over_cover.100"
g_agg5 = aggregate(data = g.temp, fhd_normal ~ ID, FUN = median)
names(g_agg5)[2] = "fhd_normal.100"

g.temp = g.temp |>
  dplyr::select(ID)
st_geometry(g.temp) = NULL
g.temp.100 = g.temp |>
  unique() |>
  left_join(g_agg1) |>
  left_join(g_agg2) |>
  left_join(g_agg3) |>
  left_join(g_agg4) |>
  left_join(g_agg5)
nrow(g.temp.100)
## 6,745, an increase of 2,458 shots over a buffer of 12.5 m
g.temp = isochrons |> 
  dplyr::select(ID)
g.temp = full_join(g.temp, g.temp.100)
setwd("E:/chapter3/for GAMs")
st_write(g.temp, "isochrons_GEDI100.gpkg", delete_dsn = T)
rm(g_agg1, g_agg2, g_agg3, g_agg4, g_agg5, g.temp.100, g.temp)

# 1000 m
g_buffer = st_buffer(gedi, dist = 1000)

# overlapping GEDI and isochrons
g.temp = st_intersection(g_buffer, isochrons)
g_agg1 = aggregate(data = g.temp, rh98 ~ ID, FUN = median)
names(g_agg1)[2] = "rh98.1000"
g_agg2 = aggregate(data = g.temp, cover ~ ID, FUN = median)
names(g_agg2)[2] = "cover.1000"
g_agg3 = aggregate(data = g.temp, cover_z_1 ~ ID, FUN = median)
names(g_agg3)[2] = "cover_z_1.1000"
g_agg4 = aggregate(data = g.temp, over_cover ~ ID, FUN = median)
names(g_agg4)[2] = "over_cover.1000"
g_agg5 = aggregate(data = g.temp, fhd_normal ~ ID, FUN = median)
names(g_agg5)[2] = "fhd_normal.1000"

g.temp = g.temp |>
  dplyr::select(ID)
st_geometry(g.temp) = NULL
g.temp.1000 = g.temp |>
  unique() |>
  left_join(g_agg1) |>
  left_join(g_agg2) |>
  left_join(g_agg3) |>
  left_join(g_agg4) |>
  left_join(g_agg5)
nrow(g.temp.1000)
## 21,854, an increase of 17,567 shots over a buffer of 12.5 m
g.temp = isochrons |> 
  dplyr::select(ID)
g.temp = full_join(g.temp, g.temp.1000)
setwd("E:/chapter3/for GAMs")
st_write(g.temp, "isochrons_GEDI1000.gpkg", delete_dsn = T)
rm(g_agg1, g_agg2, g_agg3, g_agg4, g_agg5, g.temp.1000, g.temp)

# join all
g.temp.12.5 = st_read("isochrons_GEDI12.5.gpkg")
st_geometry(g.temp.12.5) = NULL
g.temp.25 = st_read("isochrons_GEDI25.gpkg")
st_geometry(g.temp.25) = NULL
g.temp.100 = st_read("isochrons_GEDI100.gpkg")
st_geometry(g.temp.100) = NULL
g.temp.1000 = st_read("isochrons_GEDI1000.gpkg")
st_geometry(g.temp.1000) = NULL

g.temp = full_join(isochrons, g.temp.12.5) |> 
  left_join(g.temp.25) |> 
  left_join(g.temp.100) |> 
  left_join(g.temp.1000)

setwd("E:/chapter3/for GAMs")
st_write(g.temp, "isochrons_prep5.gpkg", delete_dsn = T)

# elevation ####
setwd("D:/chapter1/data")
elev = raster("proj_dem_s.tif")

setwd("E:/chapter3/for GAMs")
isochrons = st_read("isochrons_prep5.gpkg")
length(unique(isochrons$ID))
## 58,556
targetcrs = st_crs(isochrons)
iso = isochrons |> 
  dplyr::select(ID)

iso = st_transform(iso, crs = st_crs(elev))
elev = raster::extract(elev, iso, method = 'simple')
iso$elev.1 = unlist(lapply(elev, FUN = function(x) quantile(x, probs = 0.1, na.rm = T)))
iso$elev.5 = unlist(lapply(elev, FUN = function(x) median(x, na.rm = T)))
iso$elev.9 = unlist(lapply(elev, FUN = function(x) quantile(x, probs = 0.9, na.rm = T)))

st_geometry(iso) = NULL
isochrons = isochrons |> 
  left_join(iso)
st_write(isochrons, paste0("isochrons_prep6.gpkg"), delete_dsn = T)

# wind alignment ####
setwd("D:/chapter1/data")
aspect = raster("proj_dem_aspect_30m.tif")
backup = aspect
aspect = backup
slope = raster("proj_dem_slope_30m.tif")
backup2 = slope
slope = backup2
st_crs(aspect) == st_crs(slope)
## TRUE

setwd("E:/chapter3/for GAMs")
isochrons = st_read("isochrons_prep6.gpkg")
length(unique(isochrons$ID))
## 58,556
targetcrs = st_crs(isochrons)
iso = isochrons |> 
  dplyr::select(ID, winddir)

iso = st_transform(iso, crs = st_crs(aspect))
aspect = backup
aspect = raster::extract(aspect, iso[1:10,], method = 'simple')
slope = backup2
slope = raster::extract(slope, iso[1:10,], method = 'simple')
for(i in 1:length(aspect)){
  iso.temp = iso[i,]
  aspect[[i]] = (aspect[[i]] - iso.temp$winddir)*slope[[i]]
}
iso$aspect.1 = unlist(lapply(aspect, FUN = function(x) quantile(x, probs = 0.1, na.rm = T)))
iso$aspect.5 = unlist(lapply(aspect, FUN = function(x) median(x, na.rm = T)))
iso$aspect.9 = unlist(lapply(aspect, FUN = function(x) quantile(x, probs = 0.9, na.rm = T)))


iso = st_transform(iso, crs = st_crs(slope))
slope = raster::extract(slope, iso, method = 'simple')
iso$slope.1 = unlist(lapply(slope, FUN = function(x) quantile(x, probs = 0.1, na.rm = T)))
iso$slope.5 = unlist(lapply(slope, FUN = function(x) median(x, na.rm = T)))
iso$slope.9 = unlist(lapply(slope, FUN = function(x) quantile(x, probs = 0.9, na.rm = T)))

st_geometry(iso) = NULL
isochrons = isochrons |> 
  left_join(iso)
st_write(isochrons, paste0("isochrons_terrain_m2.gpkg"), delete_dsn = T)