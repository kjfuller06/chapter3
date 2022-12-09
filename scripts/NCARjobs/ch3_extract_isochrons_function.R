library(raster)
library(sf)
library(tidyverse)
library(geojsonsf)
library(exactextractr)

setwd("E:/chapter3/isochrons/Progall_ffdi_v3")
isochrons = st_read("progall_ffdi_v3.shp")
isochrons$ID = c(1:nrow(isochrons))
# isochrons$area_check = st_area(isochrons)
## measurements are the same, do not apear to be cumulative- each polygon is measured separately; the variable "area" is in km

setwd("E:/chapter3/GEDI_FESM")
# load data, create new polygons, intersecting the geometries of both
gedi = st_read("ch3_FESMandfhist.gpkg")
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
  
  # calculate the area of overlap and select only the largest overlapping progression area
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
  st_write(gedi, paste0("ch3_severity_prefire", x, ".gpkg"), delete_dsn = T)
}
ext_progfun(7)
ext_progfun(14)
ext_progfun(30)
ext_progfun(60)
ext_progfun(90)
ext_progfun(180)