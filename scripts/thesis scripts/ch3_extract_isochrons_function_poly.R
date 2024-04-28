library(raster)
library(sf)
library(tidyverse)

setwd("E:/chapter3/original/Progall_ffdi_v3")
isochrons = st_read("progall_ffdi_v3.shp")
nrow(isochrons)
## 121,432
isochrons$ID = c(1:nrow(isochrons))
# isochrons$area_check = st_area(isochrons)
## measurements are the same, do not apear to be cumulative- each polygon is measured separately; the variable "area" is in km
isochrons$time = as.POSIXct(isochrons$time)
isochrons$lasttim = as.POSIXct(isochrons$lasttim)
isochrons$progtime = difftime(isochrons$time, isochrons$lasttim, units = "hours")
isochrons$progtime = as.numeric(isochrons$progtime)
nrow(isochrons |> filter(progtime == 0))
## 4,333

touching_list = st_touches(isochrons)

isochrons$prepolyID = NA
iso = list()
for(i in c(1:nrow(isochrons))){
  iso.temp = isochrons[i,]
  pre.temp = isochrons[touching_list[[i]],]
  if(iso.temp$progtime != 0){
    pre.temp = pre.temp |> filter(time == iso.temp$lasttim)
    if(nrow(pre.temp) == 1){
      iso.temp$prepolyID = pre.temp$ID
      iso.temp$fireline = sum(st_length(st_cast(st_intersection(iso.temp, pre.temp))))
      iso[[i]] = iso.temp
    } else if(nrow(pre.temp) > 1){
      iso.temp$prepolyID = list(pre.temp$ID)
      iso.temp$fireline = sum(st_length(st_cast(st_intersection(iso.temp, pre.temp))))
      iso[[i]] = iso.temp
    } else if(nrow(pre.temp) == 0){
      print(paste0(i, " isochron does not overlap with any polygons with matching timestamps"))
    }
  }
}
iso2 = bind_rows(iso)
## ERROR: Can't combine `..13$prepolyID` <integer> and `..14$prepolyID` <list>.
iso2 = list()
for(i in 1:length(iso)){
  iso.temp = iso[[i]]
  iso.temp$prepolyID = paste(unlist(iso.temp$prepolyID), collapse = ', ')
  iso2[[i]] = iso.temp
}
iso2 = bind_rows(iso2)
nrow(iso2)
## 121,431
setwd("E:/chapter3/isochrons")
st_write(iso2, "isochrons_withfireline.gpkg", delete_dsn = T)

isochrons = iso2 |>  
  filter(progtime <= 24)
nrow(isochrons)
## 70,257
isochrons |> filter(as.numeric(fireline) == 0) |> nrow()
## 1,033 ## this is an issue
isochrons$prog = isochrons$area/isochrons$progtime/isochrons$fireline
# isochrons$prog = isochrons$area/isochrons$progtime/isochrons$fireline
st_write(isochrons, "isochrons_withfireline_f1.gpkg", delete_dsn = T)

setwd("E:/chapter3/GEDI_FESM")
# load data, create new polygons, intersecting the geometries of both
gedi = st_read("ch3_FESMandfhist_forpoly.gpkg")
targetcrs = st_crs(gedi)
gedi = st_transform(gedi, st_crs(isochrons))
g_buffer = st_buffer(gedi, dist = 12.5)
g_buffer = st_intersection(g_buffer, isochrons)
backup = g_buffer
g_buffer$TUF = as.numeric(difftime(g_buffer$time, g_buffer$DateTime, units = "days"))
g_buffer = g_buffer |> 
  filter(TUF > 0)

ext_progfun = function(x){
  g_buffer = g_buffer[g_buffer$TUF < x,]
  any(duplicated(g_buffer$shot_number))
  
  ## calculate the area of overlap and select only the largest overlapping progression area
  g_buffer$overlap = st_area(g_buffer)
  g_buffer_max = aggregate(data = g_buffer, overlap ~ shot_number, FUN = max)
  g_buffer = left_join(g_buffer_max, g_buffer)
  st_geometry(g_buffer) = "geom"
  
  # merge extracted fire progression data to GEDI info and write to file
  st_geometry(g_buffer) = NULL
  gedi$lon = st_coordinates(gedi)[,1]
  gedi$lat = st_coordinates(gedi)[,2]
  st_geometry(gedi) = NULL
  
  gedi = left_join(g_buffer, gedi)
  gedi = st_as_sf(gedi, coords = c("lon", "lat"), crs = targetcrs)
  
  gedi$time = as.POSIXct(gedi$time)
  gedi$lasttim = as.POSIXct(gedi$lasttim)
  gedi$progtime = difftime(gedi$time, gedi$lasttim, units = "hours")
  gedi$progtime = as.numeric(gedi$progtime)
  gedi = gedi |> 
    filter(progtime > 0) |> 
    filter(progtime <= 24)
  gedi$prog = gedi$area/gedi$progtime
  any(duplicated(gedi$shot_number))
  
  st_write(gedi, paste0("ch3_isochrons_prefire", x, ".gpkg"), delete_dsn = T)
}
ext_progfun(180)