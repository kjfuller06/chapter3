library(sf)
library(raster)
library(tidyverse)
library(exactextractr)
library(rgeos)
library(tmap)
library(beepr)

# combine isochrons data ####
# create index linking new and old polygon IDs
setwd("E:/chapter3/isochrons")
isochrons2 = st_read("isochrons_fireID_grouped2.gpkg")
isochrons2 = isochrons2 |> 
  dplyr::select(ID, fireID)
nrow(isochrons2)
## 121,432
length(unique(isochrons2$fireID))
## 339

ids = readRDS("isolist_IDs.rds")
st_geometry(ids) = NULL
ids = ids |> 
  dplyr::select(ID, new.ID)
nrow(ids)
## 106,057
length(unique(ids$ID))
## 96,373
length(unique(ids$new.ID))
## 33,080
## new.ID gets larger as the polygons are processed
ids = aggregate(data = ids, new.ID ~ ID, FUN = max)
nrow(ids)
## 96,373
length(unique(ids$ID))
## 96,373
length(unique(ids$new.ID))
## 31,994

isochrons2 = isochrons2 |> 
  left_join(ids)
summary(isochrons2)
length(unique(isochrons2$ID))
## 121,432
isochrons2$new.ID[is.na(isochrons2$new.ID)] = isochrons2$ID[is.na(isochrons2$new.ID)]
length(unique(isochrons2$new.ID))
## 50,715
names(isochrons2) = c("old.ID", "fireID", "ID", "geom")

# combine with original data to extract FFDI for new polygons
setwd("E:/chapter3/isochrons")
iso = st_read("isochrons_snapped_qgis.gpkg")
iso$old.ID = c(1:nrow(iso))

st_geometry(isochrons2) = NULL
iso = iso |> 
  left_join(isochrons2)
iso2 = iso
st_geometry(iso2) = NULL
iso2 = aggregate(data = iso2, ffdi_final ~ ID, FUN = mean)
iso = iso |> 
  dplyr::select(old.ID:ID) |> 
  left_join(iso2)
st_write(iso, "isochrons_grouped_newIDs.gpkg", delete_dsn = T)

isochrons2 = iso
st_geometry(isochrons2) = NULL
isochrons2 = isochrons2 |> 
  dplyr::select(-old.ID) |> 
  distinct()
isochrons2 = aggregate(data = isochrons2, fireID ~ ID + ffdi_final, FUN = max)
nrow(isochrons2)
## 50,715
length(unique(isochrons2$ID))
## 50,715

# join new fire ID and FFDI to grouped polygons
isochrons = readRDS("isolist_ind.rds")
# isochrons = st_read("isochrons_prep1.gpkg")
length(unique(isochrons$ID))
## 33,080
length(unique(isochrons$fireID))
## 284
isochrons = isochrons |> 
  dplyr::select(-fireID)

isochrons = isochrons |> 
  inner_join(isochrons2)
length(unique(isochrons$ID))
## 32,214
length(unique(isochrons$fireID))
## 142
any(duplicated(isochrons$ID))
## FALSE

isochrons.ids = isochrons |> 
  dplyr::select(ID, fireID, prepolyIDs)
saveRDS(isochrons.ids, "isolist_ind_groupingsfinal.rds")

isochrons = isochrons |> 
  dplyr::select(-prepolyIDs)
st_write(isochrons, "isochrons_ind_forextraction.gpkg", delete_dsn = T)

# merged isochrons
isochrons.merged = readRDS("isolist_merged.rds")
# isochrons.merged = st_read("isochrons_prep1.gpkg")
length(unique(isochrons.merged$ID))
## 33,080
length(unique(isochrons.merged$fireID))
## 284
isochrons.merged = isochrons.merged |> 
  dplyr::select(-fireID)

isochrons.merged = isochrons.merged |> 
  inner_join(isochrons2)
length(unique(isochrons.merged$ID))
## 32,214
length(unique(isochrons.merged$fireID))
## 142
any(duplicated(isochrons.merged$ID))
## FALSE

isochrons.merged.ids = isochrons.merged |> 
  dplyr::select(ID, fireID, prepolyIDs)
saveRDS(isochrons.merged.ids, "isolist_merged_groupingsfinal.rds")

isochrons.merged = isochrons.merged |> 
  dplyr::select(-prepolyIDs)
st_write(isochrons.merged, "isochrons.merged_merged_forextraction.gpkg", delete_dsn = T)

# GEDI ####
setwd("E:/chapter3/isochrons")
isochrons = st_read("isochrons_ind_forextraction.gpkg")
length(unique(isochrons$ID))
## 33,080
length(unique(isochrons$fireID))
## 129

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
## 109,787

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
## 5,683
g.temp = isochrons |> 
  dplyr::select(ID)
g.temp = full_join(g.temp, g.temp.12.5)
setwd("E:/chapter3/for GAMs")
st_write(g.temp, "isolist_GEDI12.5.gpkg", delete_dsn = T)
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
## 5,924
g.temp = isochrons |> 
  dplyr::select(ID)
g.temp = full_join(g.temp, g.temp.25)
setwd("E:/chapter3/for GAMs")
st_write(g.temp, "isolist_GEDI25.gpkg", delete_dsn = T)
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
## 7,250
g.temp = isochrons |> 
  dplyr::select(ID)
g.temp = full_join(g.temp, g.temp.100)
setwd("E:/chapter3/for GAMs")
st_write(g.temp, "isolist_GEDI100.gpkg", delete_dsn = T)
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
## 27
g.temp = isochrons |> 
  dplyr::select(ID)
g.temp = full_join(g.temp, g.temp.1000)
setwd("E:/chapter3/for GAMs")
st_write(g.temp, "isolist_GEDI1000.gpkg", delete_dsn = T)
rm(g_agg1, g_agg2, g_agg3, g_agg4, g_agg5, g.temp.1000, g.temp)

# join all
g.temp.12.5 = st_read("isolist_GEDI12.5.gpkg")
st_geometry(g.temp.12.5) = NULL
g.temp.25 = st_read("isolist_GEDI25.gpkg")
st_geometry(g.temp.25) = NULL
g.temp.100 = st_read("isolist_GEDI100.gpkg")
st_geometry(g.temp.100) = NULL
g.temp.1000 = st_read("isolist_GEDI1000.gpkg")
st_geometry(g.temp.1000) = NULL

g.temp = full_join(isochrons, g.temp.12.5) |> 
  left_join(g.temp.25) |> 
  left_join(g.temp.100) |> 
  left_join(g.temp.1000)

setwd("E:/chapter3/for GAMs")
st_write(g.temp, "isolist_gedi_m1.gpkg", delete_dsn = T)

# fueltype ####
setwd("E:/chapter3/for GAMs")
isochrons = st_read("isolist_gedi_m1.gpkg")
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
st_write(isochrons, "isolist_fueltype_m2.gpkg", delete_dsn = T)

# barktype ####
setwd("E:/chapter3/for GAMs")
iso = st_read("isolist_fueltype_m2.gpkg")
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
st_write(iso, "isolist_barktype_m3.gpkg", delete_dsn = T)

# LFMC ####
setwd("E:/chapter3/original/from Rachael/R project/LFMC/LFMC/GEE_LFMC/LFMC_rasters/GEE_v20May2020_RevisedMask/")
lfmc = list.files(pattern = "LFMC")
lfmc = data.frame(files = lfmc)
lfmc$date = as.POSIXct(substr(lfmc$files, 6, 15), format = "%Y_%m_%d")
## using as.Date led to problems for this extraction but the severity data extraction did fine- double-checked

setwd("E:/chapter3/for GAMs")
iso = st_read("isolist_barktype_m3.gpkg")
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
st_write(iso, "isolist_LFMC_m4.gpkg", delete_dsn = T)

# VPD ####
setwd("E:/chapter3/original/from Rachael/VPD")
vpd = list.files()
vpd = data.frame(files = vpd)
vpd$date = as.POSIXct(substr(vpd$files, 5, 12), format = "%Y%m%d")

setwd("E:/chapter3/for GAMs")
iso = st_read("isolist_LFMC_m4.gpkg")
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
st_write(iso, "isolist_VPD_m5.gpkg", delete_dsn = T)

# wind ####
setwd("E:/chapter3/for GAMs")
iso = st_read("isolist_VPD_m5.gpkg")
# iso = iso[1:100,]

# setwd("E:/chapter3/for GAMs")
# iso = st_read("isochrons_VPD_m5.gpkg")
# targetcrs = st_crs(iso)
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
write.csv(g_temp, "wind_m6.csv", row.names = F)

g_temp = g_temp |> 
  dplyr::select(-c(poly_sD, poly_eD, index))
iso = iso |> 
  left_join(g_temp, by = "ID")
st_write(iso, "isochrons_wind_m6.gpkg", delete_dsn = T)

## is wind direction related to spread direction? How similar are they? Note: wind direction will be opposite to intuitive

# # terrain ####
# setwd("D:/chapter1/data")
# slope = raster("proj_dem_slope_30m.tif")
# aspect = raster("proj_dem_aspect_30m.tif")
# elev = raster("proj_dem_s.tif")
# setwd("E:/chapter3")
# rough = raster("terrain_roughness.tif")
# 
# st_crs(slope) == st_crs(aspect)
# ## TRUE
# st_crs(slope) == st_crs(elev)
# ## TRUE
# st_crs(slope) == st_crs(rough)
# ## TRUE
# 
# setwd("E:/chapter3/for GAMs")
# isochrons = st_read("isochrons_wind_m6.gpkg")
# targetcrs = st_crs(isochrons)
# isochrons = st_transform(isochrons, crs = st_crs(aspect))
# iso = isochrons |>
#   dplyr::select(ID, spread.dir, winddir, windspeed)
# 
# iso = st_transform(iso, crs = st_crs(rough))
# rough = raster::extract(rough, iso, method = 'simple')
# iso$rough.1 = unlist(lapply(rough, FUN = function(x) quantile(x, probs = 0.1, na.rm = T)))
# iso$rough.5 = unlist(lapply(rough, FUN = function(x) median(x, na.rm = T)))
# iso$rough.9 = unlist(lapply(rough, FUN = function(x) quantile(x, probs = 0.9, na.rm = T)))
# 
# iso = st_transform(iso, crs = st_crs(elev))
# elev = raster::extract(elev, iso, method = 'simple')
# iso$elev.1 = unlist(lapply(elev, FUN = function(x) quantile(x, probs = 0.1, na.rm = T)))
# iso$elev.5 = unlist(lapply(elev, FUN = function(x) median(x, na.rm = T)))
# iso$elev.9 = unlist(lapply(elev, FUN = function(x) quantile(x, probs = 0.9, na.rm = T)))
# 
# ## subtract spread direction from aspect, essentially re-orienting the slope to be north with respect to fire spread direction
# ## NOTE: aspect direction points downslope!!
# Mode <- function(x) {
#   ux <- unique(x)
#   ux[which.max(tabulate(match(x, ux)))]
# }
# 
# i = unique(isochrons$ID)[1]
# iso.list = list()
# for(i in unique(isochrons$ID)){
#   iso.temp = isochrons |> 
#     filter(ID == i)
#   aspect.temp = raster::extract(aspect, iso.temp, method = 'simple')
#   slope.temp = raster::extract(slope, iso.temp, method = 'simple')
#   
#   # reorient slope aspect relative to fire spread direction
#   aspect.temp[[1]] = aspect.temp[[1]] - iso.temp$spread.dir
#   
#   # reorient wind direction relative to spread direction
#   iso.temp$winddir2 = iso.temp$winddir - iso.temp$spread.dir
#   
#   # correct slope aspects that are negative relative to spread direction
#   vect = aspect.temp[[1]] > 90 & aspect.temp[[1]] < 270
#   slope.temp[[1]][vect] = -slope.temp[[1]][vect]
#   
#   # calculate the net effective wind vector in the t and u directions
#   w.s.speed = iso.temp$windspeed* sin(iso.temp$winddir2*pi/180 - aspect.temp[[1]]*pi/180) + 
#               iso.temp$windspeed* cos(iso.temp$winddir2*pi/180 - aspect.temp[[1]]*pi/180)
#   slopecorrection = 150.98*tan(abs(slope.temp[[1]])*pi/180)^1.2
#   slopecorrection[vect] = -slopecorrection[vect]
#   w.s.speed = w.s.speed + slopecorrection
#   
#   iso.temp$w.s.speed = Mode(w.s.speed)
#   iso.temp$slope = Mode(slope.temp[[1]])
#   iso.temp$aspect = Mode(aspect.temp[[1]])
#   
#   iso.list[[i]] = iso.temp
# }
# iso = bind_rows(iso.list)
# 
# # iso$windspeed.cat = NA
# # iso$windspeed.cat[iso$windspeed <= 10] = "low"
# # iso$windspeed.cat[iso$windspeed > 10 & iso$windspeed < 30] = "moderate"
# # iso$windspeed.cat[iso$windspeed >= 30] = "high"
# # iso$windspeed.cat = factor(iso$windspeed.cat, levels = c("low",
# #                                                              "moderate",
# #                                                              "high"))
# # 
# # iso$aspect.cat = NA
# # iso$aspect.cat[iso$aspect > 90 & iso$aspect < 270] = "against"
# # iso$aspect.cat[iso$aspect <= 90 | iso$aspect >= 270] = "with"
# # 
# # iso$slope.cat = NA
# # iso$slope.cat[iso$slope >= 0] = "upslope"
# # iso$slope.cat[iso$slope < 0] = "downslope"
# # 
# # iso$winddir.cat = NA
# # iso$winddir.cat[iso$winddir > 90 & iso$winddir < 270] = "against"
# # iso$winddir.cat[iso$winddir <= 90 | iso$winddir >= 270] = "with"
# # 
# # ggplot(iso, aes(x = slope, y = w.s.speed)) +
# #   geom_point(aes(col = winddir.cat)) +
# #   geom_smooth(method = "lm", aes(col = winddir.cat))
# # ggplot(iso, aes(x = winddir, y = w.s.speed)) +
# #   geom_point(aes(col = aspect)) +
# #   scale_x_continuous(breaks = c(0, 90, 180, 270, 360)) + 
# #   facet_wrap(facets = "windspeed.cat")
# # ggplot(iso, aes(x = windspeed, y = w.s.speed)) +
# #   geom_point(aes(col = winddir.cat))
# 
# ## remove any slopes > 40 degrees?
# 
# st_geometry(iso) = NULL
# isochrons = isochrons |> 
#   left_join(iso)
# st_write(isochrons, "isolist_terrain_m6.gpkg", delete_dsn = T)
# 
# strahler ####
setwd("E:/chapter3/for GAMs")
iso = st_read("isolist_terrain_m6.gpkg")
targetcrs = st_crs(iso)
g = iso |> 
  dplyr::select(ID)

setwd("D:/chapter3/climatedem")
g.temp = list()
for(i in c(11:3)){
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
  # left_join(g.temp[[3]]) |> 
  left_join(g.temp[[4]]) |> 
  left_join(g.temp[[5]]) |> 
  left_join(g.temp[[6]]) |> 
  left_join(g.temp[[7]]) |> 
  left_join(g.temp[[8]]) |> 
  left_join(g.temp[[9]]) |> 
  left_join(g.temp[[10]]) |> 
  left_join(g.temp[[11]])

# g$strahler3.only[is.na(g$strahler3.only)] = 0
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
# g$s3.only = g$strahler3.only/g$area

setwd("E:/chapter3/for GAMs")
st_write(g, "isolist_strahler_m7.gpkg", delete_dsn = T)

# terrain, redo ####
setwd("D:/chapter1/data")
slope = raster("proj_dem_slope_30m.tif")
aspect = raster("proj_dem_aspect_30m.tif")
elev = raster("proj_dem_s.tif")
setwd("E:/chapter3")
rough = raster("terrain_roughness.tif")

st_crs(slope) == st_crs(aspect)
## TRUE
st_crs(slope) == st_crs(elev)
## TRUE
st_crs(slope) == st_crs(rough)
## TRUE

setwd("E:/chapter3/for GAMs")
isochrons = st_read("isolist_strahler_m7.gpkg")
targetcrs = st_crs(isochrons)
isochrons = isochrons |> 
  dplyr::select(-c(w.s.speed, slope, aspect))

isochrons = st_transform(isochrons, crs = st_crs(aspect))
iso = isochrons |>
  dplyr::select(ID, spread.dir, winddir, windspeed)

rough = raster::extract(rough, iso, method = 'simple')
iso$rough.1 = unlist(lapply(rough, FUN = function(x) quantile(x, probs = 0.1, na.rm = T)))
iso$rough.5 = unlist(lapply(rough, FUN = function(x) median(x, na.rm = T)))
iso$rough.9 = unlist(lapply(rough, FUN = function(x) quantile(x, probs = 0.9, na.rm = T)))

elev = raster::extract(elev, iso, method = 'simple')
iso$elev.1 = unlist(lapply(elev, FUN = function(x) quantile(x, probs = 0.1, na.rm = T)))
iso$elev.5 = unlist(lapply(elev, FUN = function(x) median(x, na.rm = T)))
iso$elev.9 = unlist(lapply(elev, FUN = function(x) quantile(x, probs = 0.9, na.rm = T)))

## subtract spread direction from aspect, essentially re-orienting the slope to be north with respect to fire spread direction
## NOTE: aspect direction points downslope!!

# Mode <- function(x) {
#   ux <- unique(x)
#   ux[which.max(tabulate(match(x, ux)))]
# }

i = unique(isochrons$ID)[1]
iso.list = list()
for(i in unique(isochrons$ID)){
  iso.temp = isochrons |> 
    filter(ID == i)
  aspect.temp = raster::extract(aspect, iso.temp, method = 'simple')
  slope.temp = raster::extract(slope, iso.temp, method = 'simple')
  
  # reorient slope aspect relative to fire spread direction
  aspect.temp[[1]] = aspect.temp[[1]] - iso.temp$spread.dir
  
  # reorient wind direction relative to spread direction
  iso.temp$winddir2 = iso.temp$winddir - iso.temp$spread.dir
  
  # correct slope aspects that are negative relative to spread direction
  vect = aspect.temp[[1]] > 90 & aspect.temp[[1]] < 270
  slope.temp[[1]][vect] = -slope.temp[[1]][vect]
  
  # calculate the net effective wind vector in the u direction only
  w.s.speed = iso.temp$windspeed* cos(iso.temp$winddir2*pi/180 - aspect.temp[[1]]*pi/180)
  slopecorrection = 150.98*tan(abs(slope.temp[[1]])*pi/180)^1.2
  slopecorrection[vect] = -slopecorrection[vect]
  w.s.speed = w.s.speed + slopecorrection
  
  iso.temp$w.s.up.speed = median(w.s.speed, na.rm = T)
  iso.temp$slope = median(slope.temp[[1]], na.rm = T)
  iso.temp$aspect = median(aspect.temp[[1]], na.rm = T)
  
  iso.list[[i]] = iso.temp
}
iso.list = bind_rows(iso.list)

st_geometry(iso) = NULL
st_geometry(iso.list) = NULL

isochrons = isochrons |> 
  left_join(iso) |> 
  left_join(iso.list)
st_write(isochrons, "isolist_terrain_m8.gpkg", delete_dsn = T)

# terrain, redo redo ####
setwd("D:/chapter1/data")
slope = raster("proj_dem_slope_30m.tif")
aspect = raster("proj_dem_aspect_30m.tif")

st_crs(slope) == st_crs(aspect)
## TRUE

setwd("E:/chapter3/for GAMs")
isochrons = st_read("isolist_terrain_m8.gpkg")
targetcrs = st_crs(isochrons)

isochrons = st_transform(isochrons, crs = st_crs(aspect))
iso = isochrons |>
  dplyr::select(ID, spread.dir, winddir, windspeed)

i = unique(isochrons$ID)[1]
iso.list = list()
for(i in unique(isochrons$ID)){
  iso.temp = isochrons |> 
    filter(ID == i)
  aspect.temp = raster::extract(aspect, iso.temp, method = 'simple')
  slope.temp = raster::extract(slope, iso.temp, method = 'simple')
  
  # reorient slope aspect relative to fire spread direction
  aspect.temp[[1]] = aspect.temp[[1]] - iso.temp$spread.dir
  
  # reorient wind direction relative to spread direction
  iso.temp$winddir2 = iso.temp$winddir - iso.temp$spread.dir
  
  # calculate the net effective wind vector in the u direction only
  w.s.speed = sqrt(iso.temp$windspeed^2 * sin(iso.temp$winddir2*pi/180 - aspect.temp[[1]]*pi/180)^2 + (iso.temp$windspeed* cos(iso.temp$winddir2*pi/180 - aspect.temp[[1]]*pi/180) + 150.98*tan(abs(slope.temp[[1]])*pi/180)^1.2)^2)
  
  iso.temp$w.s.speed.mag = median(w.s.speed, na.rm = T)
  
  iso.list[[i]] = iso.temp
}
iso.list = bind_rows(iso.list)

st_geometry(iso.list) = NULL

isochrons = isochrons |> 
  left_join(iso.list)
st_write(isochrons, "isolist_terrain_m9.gpkg", delete_dsn = T)

# look at NaNs ####
tmap_mode("view")
setwd("E:/chapter3/for GAMs")
iso = st_read("isolist_terrain_m9.gpkg")
iso
iso = st_make_valid(iso)
nrow(iso)
## 33,080

setwd("D:/chapter1/data")
nsw = st_read("NSW_sans_islands.shp") |> 
  st_transform(crs = st_crs(iso))

iso.yes = st_contains(nsw, iso)
iso = iso[unlist(iso.yes),]
nrow(iso)
## 32,842

# - - - wind - - - #
nrow(iso |> filter(is.na(winddir)))
## 32
tm_shape(iso |> filter(is.na(winddir))) + tm_polygons()
iso |> filter(is.na(winddir))
## all wind variables are NaN
iso = iso |> 
  filter(!is.na(winddir))
nrow(iso)
## 32,810

nrow(iso |> filter(is.na(windgust.stdev)))
## 94
tm_shape(iso |> filter(is.na(windgust.stdev))) + tm_polygons()
iso |> filter(is.na(windgust.stdev))
## few other variables NaN
iso |> filter(is.na(windgust))
## only 4 observations, worth removing
iso = iso |> 
  filter(!is.na(windgust))
nrow(iso)
## 32,806

# - - - LFMC - - - #
nrow(iso |> filter(is.na(LFMC)))
## 1,638
tm_shape(iso |> filter(is.na(LFMC))) + tm_polygons()
iso |> filter(is.na(LFMC))
## LFMC is a critical variable
iso = iso |> 
  filter(!is.na(LFMC))
nrow(iso)
## 31,168

# - - - barktypes - - - #
nrow(iso |> filter(is.na(stringybark.1)))
## 1,945
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
## 29,223

# - - - GEDI - - - #
nrow(iso |> filter(is.na(rh98.12.5)))
## 23,710
## this is the VAST majority of fire polygons!!

setwd("E:/chapter3/for GAMs")
st_write(iso, "isolist_prep1.gpkg", delete_dsn = T)

# prep ####
setwd("D:/chapter1/data")
fireregs = read.csv("fire_regimes.csv")

setwd("E:/chapter3/isochrons")
iso = st_read("isochrons_grouped_newIDs.gpkg")
iso = iso |> 
  dplyr::select(ID, ffdi_final) |> 
  distinct()
st_geometry(iso) = NULL

setwd("E:/chapter3/for GAMs")
isochrons = st_read("isolist_prep1.gpkg")
length(unique(isochrons$ID))
## 29,223

isochrons = isochrons |> filter(fueltype < 42)
isochrons = isochrons |> 
  left_join(fireregs) |> 
  left_join(iso)
isochrons = isochrons[!duplicated(isochrons$ID),] |> 
  filter(!is.na(ffdi_final))
nrow(isochrons)
## 26,424

isochrons$fire_reg[isochrons$fire_reg == 7] = 6

isochrons$prog = isochrons$area/isochrons$progtime
isochrons$logprog = log(isochrons$prog)
isochrons$fireline[isochrons$fireline == 0] = 1
isochrons$spread = isochrons$prog/isochrons$fireline
isochrons$logspread = log(isochrons$spread)

isochrons$stringybark.5[isochrons$stringybark.5 == 0.5] = 1
isochrons$stringybark.5 = as.factor(isochrons$stringybark.5)

isochrons$ribbonbark.5[isochrons$ribbonbark.5 == 0.5] = 1
isochrons$ribbonbark.5 = as.factor(isochrons$ribbonbark.5)

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

isochrons$stringybark.9[isochrons$stringybark.9 < 0.5] = 0
isochrons$stringybark.9[isochrons$stringybark.9 >= 0.5] = 1
isochrons$stringybark.9 = as.factor(isochrons$stringybark.9)

isochrons$ribbonbark.9[isochrons$ribbonbark.9 < 0.5] = 0
isochrons$ribbonbark.9[isochrons$ribbonbark.9 >= 0.5] = 1
isochrons$ribbonbark.9 = as.factor(isochrons$ribbonbark.9)

isochrons$s4.cumu = isochrons$s4.only +
  isochrons$s5.only +
  isochrons$s6.only +
  isochrons$s7.only +
  isochrons$s8.only +
  isochrons$s9.only +
  isochrons$s10.only +
  isochrons$s11.only

isochrons$s5.cumu =
  isochrons$s5.only +
  isochrons$s6.only +
  isochrons$s7.only +
  isochrons$s8.only +
  isochrons$s9.only +
  isochrons$s10.only +
  isochrons$s11.only

isochrons$s6.cumu =
  isochrons$s6.only +
  isochrons$s7.only +
  isochrons$s8.only +
  isochrons$s9.only +
  isochrons$s10.only +
  isochrons$s11.only

isochrons$s7.cumu =
  isochrons$s7.only +
  isochrons$s8.only +
  isochrons$s9.only +
  isochrons$s10.only +
  isochrons$s11.only

isochrons$s8.cumu =
  isochrons$s8.only +
  isochrons$s9.only +
  isochrons$s10.only +
  isochrons$s11.only

isochrons$s9.cumu =
  isochrons$s9.only +
  isochrons$s10.only +
  isochrons$s11.only

isochrons$s10.cumu =
  isochrons$s10.only +
  isochrons$s11.only

isochrons = na.omit(isochrons)
nrow(isochrons)
## 5,025

isochrons$abs.slope = abs(isochrons$slope)

b = isochrons
st_geometry(b) = NULL
b |> group_by(fire_reg) |> tally() |> as.data.frame()
# rainforest:                 208
# wet sclerophyll (shrubby):  706
# wet sclerophyll (grassy):   1101
# dry sclerophyll (s/g):      1136
# dry sclerophyll (shrubby):  1733
# grassy woodland:            141

st_write(isochrons, "isochrons_prep2.gpkg", delete_dsn = T)

# examine data ####
setwd("E:/chapter3/for GAMs")
iso = st_read("isochrons_prep2.gpkg")
iso
st_geometry(iso) = NULL

cor(iso |> dplyr::select(spread, prog), method = "spearman")
## 0.81