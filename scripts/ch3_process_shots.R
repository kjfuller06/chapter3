## need to run this script manually, checking each step carefully

# load data and libraries ####
library(raster)
library(sf)
library(tidyverse)
library(lubridate)

setwd("/glade/scratch/kjfuller/data/GEDI_FESM")
setwd("E:/chapter3/GEDI_FESM")
# tally fires represented in shot-fire combos- impacts ####
# calculate TSF from FESM data, filter by 180 days
gedi = st_read("ch3_shotsandFESM_fires.gpkg")
targetcrs = st_crs(gedi)
# gedi$fire_eD = as.POSIXct(gedi$fire_eD, origin = "1970-01-01") ## may not be necessary
gedi$TSF.fesm = difftime(gedi$Date, gedi$fire_eD, units = "days")
gedi$TSF.fesm = as.numeric(gedi$TSF.fesm)

shotfun = function(x){
  gedi_impacts = aggregate(data = gedi, TSF.fesm ~ shot_number, FUN = min) %>% # select the shortest TSF for each shot
    filter(TSF.fesm > x) # select shots where the fire was at least <x> days before
  gedi_impacts = gedi %>%  
    filter(shot_number %in% gedi_impacts$shot_number)
  st_write(gedi_impacts, paste0("g2_f3_shotsandFESM_impacts", x, ".gpkg"), delete_dsn = T)
  gedi = gedi_impacts  # work with a copy of the df and save the original for later processing
  gedi$lon = st_coordinates(gedi)[,1]
  gedi$lat = st_coordinates(gedi)[,2]
  st_geometry(gedi) = NULL
  
  nfires = gedi %>%
    group_by(shot_number) %>%
    tally() %>%
    as.data.frame()
  # join tally to dataset
  gedi = gedi %>%
    left_join(nfires)
  # number fires for pivoting and pivot -> data.frame with shot_number and each fire as a separate column
  gedi = gedi[order(gedi$fire_eD),]
  gedi$n = with(gedi, ave(n, shot_number, FUN = seq_along))
  nfires = gedi %>%                                          ## data.frame with fire dates in order
    dplyr::select(shot_number,
                  fire_eD,
                  n) %>%
    pivot_wider(names_from = n, values_from = fire_eD)
  
  idfires = gedi %>%                                         ## data.frame with fire IDs in order
    dplyr::select(shot_number,
                  fire_eD,
                  fire_id,
                  TSF.fesm)
  
  # join geometry back to nfires so the file can be stored as a geopackage
  gedi = gedi %>%
    dplyr::select(shot_number, lon, lat)
  nfires = nfires %>%
    left_join(gedi) %>%
    unique()
  idfires = idfires %>% 
    left_join(gedi) %>% 
    unique()
  
  nfires_write = nfires %>%
    st_as_sf(coords = c("lon", "lat"), crs = targetcrs)
  idfires_write = idfires %>% 
    st_as_sf(coords = c("lon", "lat"), crs = targetcrs)
  
  st_write(nfires_write, paste0("g2_f3_nFESM_impacts", x, ".gpkg"), delete_dsn = T)
  st_write(idfires_write, paste0("g2_f3_FESMID_impacts", x, ".gpkg"), delete_dsn = T)
  ## max 5 fires
  rm(nfires_write, idfires_write)
  nfires = st_read(paste0("g2_f3_nFESM_impacts", x, ".gpkg"))
  nfires$lon = st_coordinates(nfires)[,1]
  nfires$lat = st_coordinates(nfires)[,2]
  st_geometry(nfires) = NULL
  idfires = st_read(paste0("g2_f3_FESMID_impacts", x, ".gpkg"))
  st_geometry(idfires) = NULL
  
  # calculate difference between fires and select only fires that occurred more than 180 days apart- impacts 
  # calculate time between fires and filter by 180 days
  nfires$dT = as.numeric(difftime(nfires$X2, nfires$X1, units = "days"))
  int1 = nfires %>%
    dplyr::select(shot_number, X1, X2, dT) %>% 
    filter(abs(dT) > 180 & !is.na(nfires$X2))                 ## select fires with relevant interval data
  nfires1 = nfires %>%
    dplyr::select(shot_number, X1, X2, dT) %>%                ## select fires less than 180 days apart
    filter(dT <= 180)
  nfires$X1[nfires$shot_number %in% nfires1$shot_number] = NA ## make times for first fires NA where fires are less than 180 days apart
  ## removed 18533 fires
  
  nfires$dT = as.numeric(difftime(nfires$X3, nfires$X2, units = "days"))
  int2 = nfires %>%
    dplyr::select(shot_number, X2, X3, dT) %>% 
    filter(abs(dT) > 180 & !is.na(nfires$X3))
  nfires2 = nfires %>%
    dplyr::select(shot_number, X2, X3, dT) %>% 
    filter(abs(dT) <= 180)
  nfires$X2[nfires$shot_number %in% nfires2$shot_number] = NA ## make times for second fires NA where fires are less than 180 days apart
  ## removed 1166 fires
  
  nfires$dT = as.numeric(difftime(nfires$X4, nfires$X3, units = "days"))
  int3 = nfires %>%
    dplyr::select(shot_number, X3, X4, dT) %>%
    filter(abs(dT) >180 & !is.na(nfires$X4))
  nfires3 = nfires %>%
    dplyr::select(shot_number, X3, X4, dT) %>%
    filter(abs(dT) <= 180)
  nfires$X3[nfires$shot_number %in% nfires3$shot_number] = NA ## make times for third fires NA where fires are less than 180 days apart
  ## removed 46 fires
  
  nfires$dT = as.numeric(difftime(nfires$X5, nfires$X4, units = "days"))
  int4 = nfires %>%
    dplyr::select(shot_number, X4, X5, dT) %>%
    filter(abs(dT) > 180 & !is.na(nfires$X5))
  nfires4 = nfires %>%
    dplyr::select(shot_number, X4, X5, dT) %>%
    filter(abs(dT) <= 180)
  nfires$X4[nfires$shot_number %in% nfires4$shot_number] = NA ## make times for forth fires NA where fires are less than 180 days apart
  ## removed 2 fires
  
  names(idfires)[names(idfires) == "fire_eD"] = "value"
  nfires_long = nfires %>% 
    dplyr::select(-dT) %>%
    pivot_longer(cols = c(X1, X2, X3, X4, X5)) %>%
    na.omit() %>% 
    left_join(idfires) %>% 
    dplyr::select(-name,
                  -lon,
                  -lat)
  names(nfires_long)[names(nfires_long) == "value"] = "fire_eD"
  
  # calculate fire metrics
  nfires = nfires_long %>% 
    group_by(shot_number) %>%
    tally() %>%
    as.data.frame()
  names(nfires)[2] = "nfires.fesm"
  
  ints = full_join(int1, int2) %>%
    full_join(int3) %>%
    full_join(int4) %>%
    dplyr::select(shot_number, dT)
  min.int = aggregate(data = ints, dT ~ shot_number, FUN = min)
  names(min.int)[2] = "min.int.fesm"
  max.int = aggregate(data = ints, dT ~ shot_number, FUN = max)
  names(max.int)[2] = "max.int.fesm"
  ints = full_join(min.int, max.int)
  
  # load original shot data, filter by severity and TSF and join fesm fire metrics to shots
  gedi = gedi_impacts %>% 
    dplyr::select(-TSF.fesm)
  gedi$lon = st_coordinates(gedi)[,1]
  gedi$lat = st_coordinates(gedi)[,2]
  st_geometry(gedi) = NULL
  
  gedi = gedi %>%
    filter(shot_number %in% nfires$shot_number) %>%
    filter(fire_id %in% nfires_long$fire_id) %>% 
    left_join(nfires, by = "shot_number") %>%
    left_join(ints, by = "shot_number") %>% 
    inner_join(nfires_long)
  
  gedi = st_as_sf(gedi, coords = c("lon", "lat"), crs = targetcrs)
  st_write(gedi, paste0("g2_f3_shotsandFESM_impacts", x, "_cleaned.gpkg"), delete_dsn = TRUE)
}

shotfun(180)
shotfun(14)