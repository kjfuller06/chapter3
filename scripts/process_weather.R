library(tidyverse)
library(raster)
library(sf)
library(tmap)

setwd("D:/chapter3/data")
bom = read.csv(file = "BoMwindspeedstations.csv")
stats = full_join(stats, bom) |> 
  dplyr::select(Site, Lat, Lon)
stats$agency = NA
stats$agency[is.na(stats$Lat)] = "RFS"
stats$agency[!is.na(stats$Lat)] = "BoM"
stats = stats |> 
  dplyr::select(agency, Site)
names(stats)[2] = "stationID"
stats = stats[order(stats$agency),]
write.csv(stats, "WeatherStations.csv", row.names = F)

setwd("E:/chapter3/from Michael")
bom = st_read("available_100km_stations.shp")

wind1 = read.csv("wind_data_100km_aws_stations.csv")
## half-hour intervals
unique(wind1$winddir_quality)
## neither is quality checked
wind1$date = as.Date(wind1$date_local)
stats1 = aggregate(data = wind1, date ~ station, FUN = min)
names(stats1)[2] = "aws_sd"
stats2 = aggregate(data = wind1, date ~ station, FUN = max)
names(stats2)[2] = "aws_ed"
aws = full_join(stats1, stats2)
any(duplicated(aws$station))

wind2 = read.csv("wind_data_100km_synoptic_stations.csv")
## 3-hr intervals
unique(wind2$winddir_quality)
## neither is quality checked
wind2$date = as.Date(wind2$date_local)
stats1 = aggregate(data = wind2, date ~ station, FUN = min)
names(stats1)[2] = "syn_sd"
stats2 = aggregate(data = wind2, date ~ station, FUN = max)
names(stats2)[2] = "syn_ed"
syn = full_join(stats1, stats2)
any(duplicated(syn$station))

stats = full_join(aws, syn)
## dates match exactly where stations are present in both datasets, use aws first because of better temporal resolution

bom = st_read("available_100km_stations.shp")
targetcrs = st_crs(bom)
bom$lon = st_coordinates(bom)[,1]
bom$lat = st_coordinates(bom)[,2]
st_geometry(bom) = NULL
any(duplicated(bom$station))
## TRUE
bom = bom |> 
  dplyr::select(station, lon, lat)
bom = bom[!duplicated(bom$station),]

wind1 = read.csv("wind_data_100km_aws_stations.csv")
wind1$DateTime = as.POSIXct(paste0(wind1$date_local, " ", sprintf("%02.0f", wind1$hour_local), ":", sprintf("%02.0f", wind1$min_std)), format = "%Y-%m-%d %H:%M", tz = "UTC")
wind1 = wind1 |> 
  dplyr::select(station,
                DateTime,
                winddir,
                windspeed,
                windgust) |> 
  filter(!is.na(winddir))
wind2 = read.csv("wind_data_100km_synoptic_stations.csv")
wind2$DateTime = as.POSIXct(paste0(wind2$date_local, " ", sprintf("%02.0f", wind2$hour_local), ":", sprintf("%02.0f", wind2$min_std)), format = "%Y-%m-%d %H:%M",  tz = "UTC")
wind2 = wind2 |>
  dplyr::select(station,
                DateTime,
                winddir,
                windspeed) |> 
  filter(!is.na(winddir)) |> 
  filter(!station %in% wind1$station)
wind2$windgust = NA
wind = rbind(wind1, wind2)

wind = wind |> 
  left_join(bom)
wind = st_as_sf(wind, coords = c("lon", "lat"), crs = targetcrs)
st_write(wind, "wind_direction.gpkg", delete_dsn = T)
