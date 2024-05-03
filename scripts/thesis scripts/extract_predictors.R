library(sf)
library(raster)
library(tidyverse)
library(exactextractr)
library(rgeos)

# GEDI ####
setwd("E:/chapter3/isochrons")
isochrons = st_read("isochrons_withfireline_f1.gpkg")
length(unique(isochrons$ID))
## 70,370
setwd("E:/chapter3/GEDI_FESM")
# load data, create new polygons, intersecting the geometries of both
gedi = st_read("ch3_FESMandfhist_forpoly.gpkg")
targetcrs = st_crs(gedi)
gedi = st_transform(gedi, st_crs(isochrons))
g_buffer = st_buffer(gedi, dist = 12.5)
g_buffer = st_intersection(g_buffer, isochrons)
length(unique(g_buffer$ID))
## 5,061
backup = g_buffer
g_buffer$TUF = as.numeric(difftime(g_buffer$time, g_buffer$DateTime, units = "days"))
g_buffer = g_buffer |> 
  filter(TUF > 0)
length(unique(g_buffer$ID))
## 4,726

gedi = g_buffer |> 
  dplyr::select(ID, rh98:over_cover)
st_geometry(gedi) = NULL

# calculate the median of continuous variables
g_agg1 = aggregate(data = gedi, rh98 ~ ID, FUN = median)
names(g_agg1)[2] = "rh98"
g_agg1.1 = aggregate(data = gedi, rh98 ~ ID, FUN = function(x) quantile(x, probs = 0.1, na.rm = T))
names(g_agg1.1)[2] = "rh98.1"
g_agg1.9 = aggregate(data = gedi, rh98 ~ ID, FUN = function(x) quantile(x, probs = 0.9, na.rm = T))
names(g_agg1.9)[2] = "rh98.9"
g_agg2 = aggregate(data = gedi, cover ~ ID, FUN = median)
names(g_agg2)[2] = "cover"
g_agg2.1 = aggregate(data = gedi, cover ~ ID, FUN = function(x) quantile(x, probs = 0.1, na.rm = T))
names(g_agg2.1)[2] = "cover.1"
g_agg2.9 = aggregate(data = gedi, cover ~ ID, FUN = function(x) quantile(x, probs = 0.9, na.rm = T))
names(g_agg2.9)[2] = "cover.9"
g_agg3 = aggregate(data = gedi, cover_z_1 ~ ID, FUN = median)
names(g_agg3)[2] = "cover_z_1"
g_agg3.1 = aggregate(data = gedi, cover_z_1 ~ ID, FUN = function(x) quantile(x, probs = 0.1, na.rm = T))
names(g_agg3.1)[2] = "cover_z_1.1"
g_agg3.9 = aggregate(data = gedi, cover_z_1 ~ ID, FUN = function(x) quantile(x, probs = 0.9, na.rm = T))
names(g_agg3.9)[2] = "cover_z_1.9"
g_agg4 = aggregate(data = gedi, over_cover ~ ID, FUN = median)
names(g_agg4)[2] = "over_cover"
g_agg4.1 = aggregate(data = gedi, over_cover ~ ID, FUN = function(x) quantile(x, probs = 0.1, na.rm = T))
names(g_agg4.1)[2] = "over_cover.1"
g_agg4.9 = aggregate(data = gedi, over_cover ~ ID, FUN = function(x) quantile(x, probs = 0.9, na.rm = T))
names(g_agg4.9)[2] = "over_cover.9"
g_agg5 = aggregate(data = gedi, fhd_normal ~ ID, FUN = median)
names(g_agg5)[2] = "fhd_normal"
g_agg5.1 = aggregate(data = gedi, fhd_normal ~ ID, FUN = function(x) quantile(x, probs = 0.1, na.rm = T))
names(g_agg5.1)[2] = "fhd_normal.1"
g_agg5.9 = aggregate(data = gedi, fhd_normal ~ ID, FUN = function(x) quantile(x, probs = 0.9, na.rm = T))
names(g_agg5.9)[2] = "fhd_normal.9"

g = gedi |> 
  dplyr::select(ID) |> 
  unique() |> 
  left_join(g_agg1) |> 
  left_join(g_agg1.1) |> 
  left_join(g_agg1.9) |> 
  left_join(g_agg2) |>
  left_join(g_agg2.1) |> 
  left_join(g_agg2.9) |> 
  left_join(g_agg3) |>
  left_join(g_agg3.1) |> 
  left_join(g_agg3.9) |> 
  left_join(g_agg4) |> 
  left_join(g_agg4.1) |> 
  left_join(g_agg4.9) |> 
  left_join(g_agg5) |> 
  left_join(g_agg5.1) |> 
  left_join(g_agg5.9)
isochrons = left_join(isochrons, g, by = "ID")
nrow(isochrons |> filter(is.na(rh98)))
## 65,644
nrow(isochrons |> filter(!is.na(rh98)))
## 4,726

setwd("E:/chapter3/for GAMs")
st_write(isochrons, "isochrons_gedi_m1.gpkg", delete_dsn = T)

# terrain ####
setwd("D:/chapter1/data")
slope = raster("proj_dem_slope_30m.tif")
setwd("E:/chapter3")
rough = raster("terrain_roughness.tif")

setwd("E:/chapter3/for GAMs")
isochrons = st_read("isochrons_gedi_m1.gpkg")
targetcrs = st_crs(isochrons)

isochrons = st_transform(isochrons, crs = st_crs(rough))
rough = raster::extract(rough, isochrons, method = 'simple')
isochrons$rough.1 = unlist(lapply(rough, FUN = function(x) quantile(x, probs = 0.1, na.rm = T)))
isochrons$rough.5 = unlist(lapply(rough, FUN = function(x) median(x, na.rm = T)))
isochrons$rough.9 = unlist(lapply(rough, FUN = function(x) quantile(x, probs = 0.9, na.rm = T)))

isochrons = st_transform(isochrons, crs = st_crs(slope))
slope = raster::extract(slope, isochrons, method = 'simple')
isochrons$slope.1 = unlist(lapply(slope, FUN = function(x) quantile(x, probs = 0.1, na.rm = T)))
isochrons$slope.5 = unlist(lapply(slope, FUN = function(x) median(x, na.rm = T)))
isochrons$slope.9 = unlist(lapply(slope, FUN = function(x) quantile(x, probs = 0.9, na.rm = T)))

isochrons = st_transform(isochrons, crs = targetcrs)
st_write(isochrons, paste0("isochrons_terrain_m2.gpkg"), delete_dsn = T)

# fueltype ####
setwd("E:/chapter3/for GAMs")
g = st_read("isochrons_terrain_m2.gpkg")
targetcrs = st_crs(g)

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

g$fueltype = unlist(lapply(fueltype, getmode))
g = g |> 
  left_join(fire_reg)

g = st_transform(g, crs = targetcrs)
setwd("E:/chapter3/for GAMs")
st_write(g, "isochrons_fueltype_m3.gpkg", delete_dsn = T)

# barktype ####
setwd("E:/chapter3/for GAMs")
iso = st_read("isochrons_fueltype_m3.gpkg")
targetcrs = st_crs(iso)
g = iso |>
  dplyr::select(ID)

setwd("D:/chapter1/Major outputs/final maps")
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
st_write(g, "isochrons_barktype_m4.gpkg", delete_dsn = T)

# LFMC ####
setwd("E:/chapter3/from Rachael/R project/LFMC/LFMC/GEE_LFMC/LFMC_rasters/GEE_v20May2020_RevisedMask/")
lfmc = list.files()
lfmc = data.frame(files = lfmc)
lfmc$date = as.POSIXct(substr(lfmc$files, 6, 15), format = "%Y_%m_%d")
## using as.Date led to problems for this extraction but the severity data extraction did fine- double-checked

setwd("E:/chapter3/for GAMs")
iso = st_read("isochrons_barktype_m4.gpkg")
targetcrs = st_crs(iso)
g = iso |>
  dplyr::select(ID,
                poly_sD,
                poly_eD)

setwd("E:/chapter3/from Rachael/R project/LFMC/LFMC/GEE_LFMC/LFMC_rasters/GEE_v20May2020_RevisedMask/")
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
st_geometry(g_temp) = NULL
iso = iso |> 
  left_join(g_temp, by = "ID")

setwd("E:/chapter3/for GAMs")
st_write(iso, "isochrons_LFMC_m5.gpkg", delete_dsn = T)

# VPD ####
setwd("E:/chapter3/from Rachael/VPD")
vpd = list.files()
vpd = data.frame(files = vpd)
vpd$date = as.POSIXct(substr(vpd$files, 5, 12), format = "%Y%m%d")

setwd("E:/chapter3/for GAMs")
iso = st_read("isochrons_LFMC_m5.gpkg")
targetcrs = st_crs(iso)
g = iso |> 
  filter(ID)

setwd("E:/chapter3/from Rachael/VPD")
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

iso = iso |> 
  left_join(g_temp, by = "ID")
st_write(g, "isochrons_wind_m7.gpkg", delete_dsn = T)

# strahler ####
setwd("E:/chapter3/isochrons")
# iso = st_read("isochrons_withfireline_f1.gpkg")
iso = st_read("isochrons_wind_m7.gpkg")
targetcrs = st_crs(iso)
g = iso |> 
  dplyr::select(ID)

setwd("D:/chapter3/climatedem")
for(i in c(12:3)){
  i.temp = as.character(c(i:12))
  s_all = list()
  for(x in i.temp){
    s.temp = st_read(paste0("strahler", x, ".gpkg")) |> st_transform(crs = targetcrs)
    names(s.temp)[1:2] = c("cat", "value")
    s_all[[x]] = s.temp
  }
  s_all = bind_rows(s_all)
  
  s.temp = st_intersection(s_all, g)
  if(nrow(s.temp) > 0){
    s.temp$length = as.numeric(st_length(s.temp))
    st_geometry(s.temp) = NULL
    s.temp = aggregate(data = s.temp, length ~ ID, FUN = sum)
    s.temp$length[is.na(s.temp$length)] = 0
    names(s.temp)[2] = paste0("strahler", i)
    
    g = g |> 
      left_join(s.temp, by = "ID")
  }
}
st_geometry(g) = NULL
iso = iso |> 
  left_join(g, by = "ID")
st_write(iso, paste0("isochrons_strahler_m8.gpkg"), delete_dsn = T)

