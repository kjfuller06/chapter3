## need to run this script manually, checking each step carefully

# load data and libraries ####
library(raster)
library(sf)
library(tidyverse)
library(lubridate)

# setwd("/glade/scratch/kjfuller/data/GEDI_FESM")
setwd("E:/chapter3/GEDI_FESM")
# select unburned sites that were sampled no more than <x> days before fire ####
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
gedi_unburnt = st_read("ch3_shotsandFESM_5yrsunburnt.gpkg")

shotfun = function(x){
  gedi_prefire = aggregate(data = gedi_unburnt, TUF.fesm ~ shot_number, FUN = min) %>% # select the shortest TSF for each shot
    filter(TUF.fesm < x) # select shots where the GEDI sample was no more than <x> days before fire
  gedi_prefire = gedi_unburnt %>%  
    filter(shot_number %in% gedi_prefire$shot_number)
  st_write(gedi_prefire, paste0("ch3_shotsandFESM_prefire", x, ".gpkg"), delete_dsn = T)
}

shotfun(7)
shotfun(14)
shotfun(30)
shotfun(60)
shotfun(90)
shotfun(180)


# merge with fhist ####
setwd("E:/chapter3/GEDI_fhist")
fhist = st_read("ch3_f4_fhist.gpkg")
targetcrs = st_crs(fhist)
fhist$lon = st_coordinates(fhist)[,1]
fhist$lat = st_coordinates(fhist)[,2]
st_geometry(fhist) = NULL

mergefun = function(x){
  setwd("E:/chapter3/GEDI_FESM")
  g_temp = st_read(paste0("ch3_shotsandFESM_prefire", x, ".gpkg"))
  g_temp$lon = st_coordinates(g_temp)[,1]
  g_temp$lat = st_coordinates(g_temp)[,2]
  st_geometry(g_temp) = NULL
  
  g_temp = left_join(g_temp, fhist_temp) |> 
    filter(TSF.hist >= 5 | is.na(TSF.hist))
  g_temp = st_as_sf(g_temp, coords = c("lon", "lat"), crs = targetcrs)
  st_write(g_temp, paste0("Ch3_allfiredata_prefire", x, ".gpkg"), delete_dsn = T)
}

mergefun(7)
mergefun(14)
mergefun(30)
mergefun(60)
mergefun(90)
mergefun(180)
