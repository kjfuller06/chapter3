# some summary stats to run on metadata first, on dates later
library(raster)
library(sf)
library(tidyverse)
library(tmap)
library(geojsonsf)
library(lme4)
library(lmerTest)

setwd("/glade/scratch/kjfuller/data/GEDI_FESM")
# create file with all shots and fires ####
fesm_md = read.csv("fesm_metadata_preFire.csv")
fires = c()
for(i in fesm_md$id){
  fires = c(fires, list.files(pattern = as.character(i)))
}
fires = fires[grepl(".gpkg", fires)]

gedi = st_read(fires[1])
targetcrs = st_crs(gedi)
gedi$lon = st_coordinates(gedi)[,1]
gedi$lat = st_coordinates(gedi)[,2]
st_geometry(gedi) = NULL
code = substr(fires[1], 11, 16)
gedi$fire_id = code
gedi$fire_sD = as.Date(fesm_md$fireED[fesm_md$id == code])
gedi = gedi %>% 
  filter(Date < fire_sD)
for(i in c(2:length(fires))){
  g = st_read(fires[i])
  g$lon = st_coordinates(g)[,1]
  g$lat = st_coordinates(g)[,2]
  st_geometry(g) = NULL
  code = substr(fires[i], 11, 16)
  g$fire_id = code
  g$fire_eD = as.Date(fesm_md$fireED[fesm_md$id == code])
  g = g %>% 
    filter(Date > fire_eD)
  if(nrow(g) > 0){
    gedi = full_join(gedi, g)
  }
}
gedi2 = st_as_sf(gedi, coords = c("lon", "lat"), crs = targetcrs)
st_write(gedi2, "g2_f3_prefireshots.gpkg", delete_dsn = TRUE)
