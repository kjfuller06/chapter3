## need to run this script manually, checking each step carefully

# load data and libraries ####
library(raster)
library(sf)
library(tidyverse)
library(lubridate)

# setwd("/glade/scratch/kjfuller/data/GEDI_FESM")
setwd("E:/chapter3/GEDI_FESM")
# select burned sites that were sampled no more than <x> days before fire ####
# # calculate TSF from FESM data, filter by 180 days
# gedi = st_read("ch3_shotsandFESM_fires.gpkg")
# targetcrs = st_crs(gedi)
# 
# gedi_unburnt = aggregate(data = gedi, TSF.fesm ~ shot_number, FUN = max) %>% # select only shots that burned at least 5 years before
#   filter(TSF.fesm < 0 | TSF.fesm > 1825) 
# gedi_unburnt = gedi %>% 
#   filter(shot_number %in% gedi_unburnt$shot_number) |> 
#   filter(TUF.fesm > 0)
# any(duplicated(gedi_unburnt$shot_number))
# ## TRUE
# 
# dups = gedi_unburnt[duplicated(gedi_unburnt$shot_number),]
# dups = gedi_unburnt |> 
#   filter(shot_number %in% dups$shot_number)
# nrow(dups)
# ## duplicates must be based on multiple fires following shot collection
# 
# gedi_unburnt = gedi_unburnt[order(gedi_unburnt$TUF.fesm),]
# gedi_unburnt$n = NA
# gedi_unburnt$n = with(gedi_unburnt, ave(n, shot_number, FUN = seq_along))
# gedi_unburnt = gedi_unburnt |> 
#   filter(n == 1)
# any(duplicated(gedi_unburnt$shot_number))
# ## FALSE
# 
# st_write(gedi_unburnt, "ch3_shotsandFESM_5yrsunburnt.gpkg")
g_temp = st_read("ch3_shotsandFESM_5yrsunburnt.gpkg")
targetcrs = st_crs(g_temp)
g_temp$lon = st_coordinates(g_temp)[,1]
g_temp$lat = st_coordinates(g_temp)[,2]
st_geometry(g_temp) = NULL

# merge with fhist ####
setwd("E:/chapter3/GEDI_fhist")
fhist = st_read("ch3_f4_fhist.gpkg")
st_geometry(fhist) = NULL

setwd("E:/chapter3/GEDI_FESM")
g_temp = left_join(g_temp, fhist) |> 
  filter(TSF.hist >= 5 | is.na(TSF.hist))
g_temp = st_as_sf(g_temp, coords = c("lon", "lat"), crs = targetcrs)
st_write(g_temp, paste0("Ch3_FESMandfhist.gpkg"), delete_dsn = T)

# select unburned sites ####
# calculate TSF from FESM data, filter by 180 days
gedi_unburnt = st_read("ch3_shotsandFESM_nofires.gpkg")
targetcrs = st_crs(gedi_unburnt)

any(duplicated(gedi_unburnt$shot_number))
## TRUE

gedi_unburnt = gedi_unburnt[!duplicated(gedi_unburnt$shot_number),]
## remove duplicates- data are only for FESM fires, which did not occur for these shots
any(duplicated(gedi_unburnt$shot_number))
## FALSE
names(gedi_unburnt)
gedi_unburnt$fire_eD = NA
gedi_unburnt$fire_sD = NA
gedi_unburnt$fire_id = NA
gedi_unburnt$TSF.fesm = NA
gedi_unburnt$TUF.fesm = NA

st_write(gedi_unburnt, "ch3_shotsandFESM_unburnt.gpkg")

# merge with fhist ####
setwd("E:/chapter3/GEDI_fhist")
fhist = st_read("ch3_f4_fhist.gpkg")
st_geometry(fhist) = NULL

setwd("E:/chapter3/GEDI_FESM")
g_temp = st_read("ch3_shotsandFESM_unburnt.gpkg")
targetcrs = st_crs(g_temp)
g_temp$lon = st_coordinates(g_temp)[,1]
g_temp$lat = st_coordinates(g_temp)[,2]
st_geometry(g_temp) = NULL

g_temp = left_join(g_temp, fhist) |> 
  filter(TSF.hist >= 5 | is.na(TSF.hist))
g_temp = st_as_sf(g_temp, coords = c("lon", "lat"), crs = targetcrs)
g_temp = g_temp |> 
  filter(!shot_number %in% g_fires$shot_number)

g_fires = st_read("Ch3_FESMandfhist.gpkg") |> 
  dplyr::select(-n)
names(g_fires)[names(g_fires) == "geom"] = "geometry"
st_geometry(g_fires) = "geometry"

g_temp = rbind(g_temp, g_fires)
any(duplicated(g_temp$shot_number))

st_write(g_temp, paste0("Ch3_FESMandfhist_forpoly.gpkg"), delete_dsn = T)
