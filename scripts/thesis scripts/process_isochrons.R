library(raster)
library(sf)
library(tidyverse)
library(tmap)
# library(spdep)

# # validate geometries ####
# setwd("E:/chapter3/original/Progall_ffdi_v3")
# isochrons = st_read("progall_ffdi_v3.shp")
# nrow(isochrons)
# ## 121,432
# isochrons = st_make_valid(isochrons)
# all(st_is_valid(isochrons))
# nrow(isochrons)
# ## 121,432
# setwd("E:/chapter3/isochrons")
# st_write(isochrons, "isochrons_valid.gpkg", delete_dsn = T)
# 
# # check methods ####
# tmap_options(check.and.fix = T)
# tmap_mode("view")
# setwd("E:/chapter3/isochrons")
# original = st_read("isochrons_snapped_qgis.gpkg")
# nrow(original)
# ## 121,432
# original$ID = c(1:nrow(original))
# # original$area_check = st_area(original)
# ## measurements are the same, do not apear to be cumulative- each polygon is measured separately; the variable "area" is in km
# original$time = as.POSIXct(original$time)
# original$lasttim = as.POSIXct(original$lasttim)
# original$progtime = difftime(original$time, original$lasttim, units = "hours")
# original$progtime = as.numeric(original$progtime)
# 
# setwd("E:/chapter3/isochrons")
# isochrons = st_read("isochrons_withfireline.gpkg")
# isochrons |> filter(progtime <= 24) |>  filter(as.numeric(fireline) == 0 | is.na(as.numeric(fireline))) |> nrow()
# ## 1,146 ## this is an issue
# 
# issue = isochrons |> filter(progtime <= 24) |> filter(as.numeric(fireline) == 0 | is.na(as.numeric(fireline)))
# issue = issue |> filter(is.na(as.numeric(fireline)))
# i = issue$ID[2]
# iso.temp = isochrons |> filter(ID == i) |> dplyr::select(-c(prepolyID, fireline))
# iso.buff = st_buffer(iso.temp, dist = 1000)
# tm_shape(iso.buff) + tm_polygons(col = "blue") + tm_shape(iso.temp) + tm_polygons()
# 
# pre.temp = original[iso.buff,]
# tm_shape(iso.buff) + tm_polygons(col = "blue") + tm_shape(iso.temp) + tm_polygons() +
#   tm_shape(pre.temp) + tm_polygons()
# 
# pre.temp = pre.temp |> filter(time == iso.temp$lasttim)
# tm_shape(iso.temp) + tm_polygons(col = "blue") + tm_shape(pre.temp) + tm_polygons()
# 
# st_is_valid(pre.temp)
# 
# iso.temp$prepolyID = list(pre.temp$ID)
# centerline = st_cast(st_intersection(iso.temp, pre.temp))
# tm_shape(iso.temp) + tm_polygons(col = "blue") + tm_shape(pre.temp) + tm_polygons() + tm_shape(centerline) + tm_lines(col = "red")
# iso.temp$fireline = sum(st_length(st_cast(st_intersection(iso.temp, pre.temp))))
# 
# calculate fireline length for adjacent polygons ####
setwd("E:/chapter3/isochrons")
isochrons = st_read("isochrons_snapped_qgis.gpkg")
isochrons$ID = c(1:nrow(isochrons))
targetcrs = st_crs(isochrons)
# isochrons$area_check = st_area(isochrons)
## measurements are the same, do not apear to be cumulative- each polygon is measured separately; the variable "area" is in km
isochrons$time = as.POSIXct(isochrons$time)
isochrons$lasttim = as.POSIXct(isochrons$lasttim)
isochrons$progtime = difftime(isochrons$time, isochrons$lasttim, units = "hours")
isochrons$progtime = as.numeric(isochrons$progtime)
nrow(isochrons |> filter(progtime == 0))
## 4,333

# touching_list = st_touches(isochrons)
# saveRDS(touching_list, "touchinglist.rds")
touching_list = readRDS("touchinglist.rds")

isochrons$prepolyID = NA
iso = list()
# i = 35 makes a good example
for(i in c(1:nrow(isochrons))){
  iso.temp = isochrons[i,]
  iso.centroid = st_transform(st_centroid(iso.temp), crs = 4326)
  touch.temp = isochrons[touching_list[[i]],]
  
  if(iso.temp$progtime != 0){
    pre.temp = touch.temp |> filter(time == iso.temp$lasttim)
    if(nrow(pre.temp) == 0){
      print(paste0(i, " isochron does not overlap with any polygons with matching timestamps"))
      iso.temp$prepolyID = NA
      iso.temp$fireline = NA
      iso.temp$spreaddir = NA
      iso[[i]] = iso.temp
    } else {
      
      pre.line = st_cast(st_intersection(iso.temp, pre.temp))
      iso.temp$fireline = sum(st_length(pre.line))
      pre.line = st_cast(pre.line, "POINT")
      
      if(nrow(pre.line) == 1){
        pre.centroid = pre.line
      } else if(nrow(pre.line) < 4){
        pre.poly = rbind(pre.line, pre.line[nrow(pre.line):1,])

        pre.centroid = pre.poly |> 
          summarise(geom = st_combine(geom)) %>%
          st_cast("POLYGON") |> 
          st_centroid()
        
      } else if(nrow(pre.line) >= 4){
        pre.centroid = pre.line |> 
          summarise(geom = st_combine(geom)) %>%
          st_cast("POLYGON") |> 
          st_centroid()
      }
      
      pre.centroid = st_transform(pre.centroid, crs = 4326)
      iso.temp$spreaddir = as.numeric(mean((nngeo::st_azimuth(pre.centroid, iso.centroid))))
      
      if(nrow(pre.temp) == 1){
        iso.temp$prepolyID = pre.temp$ID
      } else if(nrow(pre.temp) > 1){
        iso.temp$prepolyID = list(pre.temp$ID)
      }
      iso[[i]] = iso.temp
    } 
  }
}

iso2 = list()
for(i in 1:length(iso)){
  iso.temp = iso[[i]]
  iso.temp$prepolyID = paste(unlist(iso.temp$prepolyID), collapse = ', ')
  iso2[[i]] = iso.temp
}
iso2 = bind_rows(iso2)
nrow(iso2)
# 121,431
setwd("E:/chapter3/isochrons")
st_write(iso2, "isochrons_withfireline.gpkg", delete_dsn = T)

iso2 = st_read("isochrons_withfireline.gpkg")
all(unique(iso2$ID) %in% unique(isochrons$ID))
## FALSE
length(unique(iso2$ID)) == length(unique(isochrons$ID))
## FALSE

missing = isochrons |> filter(!(ID %in% iso2$ID))
all(missing$time == missing$lasttim)
## TRUE
## so all missing ID's from the original dataset correspond to first fire polygons

st_geometry(iso2) = NULL
iso2 = iso2 |> 
  filter(!is.na(fireline) & progtime != 0)
nrow(iso2)
## 117,099
cor(iso2 |> dplyr::select(area, fireline), method = "spearman")
## 0.85

isochrons = backup
isochrons$prog = isochrons$area/isochrons$progtime
isochrons$spread = isochrons$area/isochrons$progtime/isochrons$fireline
nrow(isochrons |> filter(is.infinite(spread)))
## 3,032
## may just have to remove these
summary(isochrons |> filter(is.infinite(spread)))
any(duplicated(isochrons$ID[is.infinite(isochrons$spread)]))
## FALSE

isochrons$fireline = as.numeric(isochrons$fireline)
isochrons$fireline[isochrons$fireline == 0] = 1
isochrons$spread = isochrons$area/isochrons$progtime/isochrons$fireline
summary(isochrons)

isochrons = isochrons |>
  filter(progtime <= 24)
nrow(isochrons)
## 70,370
# st_write(isochrons, "isochrons_withfireline_f1.gpkg", delete_dsn = T)
st_write(isochrons, "isochrons_withfireline_withspreaddir.gpkg", delete_dsn = T)
st_geometry(isochrons) = NULL
isochrons = isochrons |> 
  filter(!is.na(fireline))
cor(isochrons |> dplyr::select(area, fireline, spread), method = "spearman")
## 0.89, 0.65, 0.45

iso3 = st_read("isochrons_gedi_m1.gpkg")
backup2 = iso3
st_geometry(iso3) = NULL
iso3 = iso3 |> 
  filter(!is.na(fireline))
nrow(iso3)
## 70,370
cor(iso3 |> dplyr::select(area, fireline, spread), method = "spearman")
## 0.89, 0.65, 0.45
## looks the same

nans = isochrons |> filter(is.na(spreaddir) & !is.na(spread))
nrow(nans)
## 2,925

iso4 = st_read("isochrons_snapped_qgis.gpkg")
iso4$ID = c(1:nrow(iso4))

tm_shape(nans) + tm_polygons()
tm_shape(nans[5,]) + tm_polygons(col = "red") + tm_shape(iso4 |> filter(ID == 135)) + tm_polygons()
## all isochrons with NaN spreaddir is because the polygon is surrounded by the previous polygon- so the centerpoint for the fireline is actually inside the fire polygon