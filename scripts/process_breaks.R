library(raster)
library(sf)
library(tidyverse)
library(geojsonsf)
library(rjson)

setwd("E:/chapter3/isochrons/Progall_ffdi_v3")
isochrons = st_read("progall_ffdi_v3.shp")
isochrons$ID = c(1:nrow(isochrons))
isochrons$time = as.POSIXct(isochrons$time)
isochrons$lasttim = as.POSIXct(isochrons$lasttim)
## last observation date was 2020-03-02
# isochrons$area_check = st_area(isochrons)
## measurements are the same, do not apear to be cumulative- each polygon is measured separately; the variable "area" is in km

# load predictor variables
setwd("E:/chapter3/railways/")
rail = geojson_sf("Railway_EPSG4326_edit.json")
# rail$enddate = as.POSIXct(substr(rail$enddate, 1, 8), format = "%Y%m%d")
# summary(rail$enddate)
## end dates all after the year 3000
rail$startdate = as.POSIXct(substr(rail$startdate, 1, 8), format = "%Y%m%d")
summary(rail$startdate)
rail = rail |> 
  filter(startdate < "2020-03-02")
## type 3 is "intunnel" based on online portal data check
rail = rail |> 
  filter(railontype != 3)

setwd("E:/chapter3/waterways/")
water = geojson_sf("HydroLine_EPSG4326_edit.json")
water$enddate = as.POSIXct(substr(water$enddate, 1, 8), format = "%Y%m%d")
summary(water$enddate)
# # end dates all after the year 3000
water$startdate = as.POSIXct(substr(water$startdate, 1, 8), format = "%Y%m%d")
summary(water$startdate)
water = water |> 
  filter(startdate < "2020-03-02")
head(water)
## perenniality of 1 == "perennial"
## perenniality of 2 == "non-perennial"
## perenniality of 3 == "mainly dry
water = water |> 
  filter(perenniality == 1)
## tunnel-siphons (underground waterways) are not listed as perennial, no need to filter

setwd("E:/chapter3/fire lines/")
firelines = geojson_sf("ClassifiedFireTrail_EPSG4326_edit.json")
# firelines$enddate = as.POSIXct(substr(firelines$enddate, 1, 8), format = "%Y%m%d")
# summary(firelines$enddate)
# end dates all after the year 3000
firelines$startdate = as.POSIXct(substr(firelines$startdate, 1, 8), format = "%Y%m%d")
summary(firelines$startdate)
firelines = firelines |> 
  filter(startdate < "2020-03-02")

setwd("E:/chapter3/roadways/")
roads = geojson_sf("RoadSegment_EPSG4326_edit.json")
# roads$enddate = as.POSIXct(substr(roads$enddate, 1, 8), format = "%Y%m%d")
# summary(roads$enddate)
# end dates all after the year 3000
roads$startdate = as.POSIXct(substr(roads$startdate, 1, 8), format = "%Y%m%d")
summary(roads$startdate)
roads = roads |> 
  filter(startdate < "2020-03-02")
## type 3 is "intunnel" based on online portal data check
roads = roads |> 
  filter(railontype != 3)