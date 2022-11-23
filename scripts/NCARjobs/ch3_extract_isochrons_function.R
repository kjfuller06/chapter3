library(raster)
library(sf)
library(tidyverse)
library(geojsonsf)
library(exactextractr)

setwd("E:/chapter3/isochrons/Progall_ffdi_v3")
isochrons = st_read("progall_ffdi_v3.shp")
isochrons$ID = c(1:nrow(isochrons))

# setwd("/glade/scratch/kjfuller/data/GEDI")
setwd("E:/chapter3/GEDI_FESM")
ext_progfun = function(x){
  # load data, create new polygons, intersecting the geometries of both
  gedi = st_read(paste0("ch3_allfiredata_prefire", x, ".gpkg"))
  targetcrs = st_crs(gedi)
  gedi = st_transform(gedi, st_crs(isochrons))
  g_buffer = st_buffer(gedi, dist = 12.5)
  g_buffer = st_intersection(g_buffer, isochrons)
  any(duplicated(g_buffer$shot_number))
  
  # select the shots that overlap more than one fire progression area
  dups = g_buffer[duplicated(g_buffer$shot_number),]
  dups = g_buffer |> 
    filter(shot_number %in% dups$shot_number)
  # calculate the area of overlap and select only the largest overlapping progression area
  dups$overlap = st_area(dups)
  dups_max = aggregate(data = dups, overlap ~ shot_number, FUN = max)
  dups = left_join(dups_max, dups)
  st_geometry(dups) = "geom"
  
  # join all data together again
  dups = dups |> 
    dplyr::select(-overlap)
  g_buffer = g_buffer |> 
    filter(!shot_number %in% dups$shot_number)
  g_buffer = rbind(g_buffer, dups)
  any(duplicated(g_buffer$shot_number))
  
  # merge extracted fire progression data to GEDI info and write to file
  st_geometry(g_buffer) = NULL
  gedi$lon = st_coordinates(gedi)[,1]
  gedi$lat = st_coordinates(gedi)[,2]
  st_geometry(gedi) = NULL
  
  gedi = left_join(g_buffer, gedi)
  gedi = st_as_sf(gedi, coords = c("lon", "lat"), crs = targetcrs)
  st_write(gedi, paste0("ch3_isochrons_prefire", x, ".gpkg"), delete_dsn = T)
}

ext_progfun(7)
ext_progfun(14)
ext_progfun(30)
ext_progfun(60)
ext_progfun(90)
ext_progfun(180)
