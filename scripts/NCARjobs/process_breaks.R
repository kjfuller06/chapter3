library(raster)
library(sf)
library(tidyverse)
library(geojsonsf)
library(rjson)

setwd("/glade/scratch/kjfuller/data")
# # setwd("E:/chapter3/fire lines/")
# firelines = geojson_sf("ClassifiedFireTrail_EPSG4326_edit.json")
# firelines$startdate = as.POSIXct(substr(firelines$startdate, 1, 8), format = "%Y%m%d")
# firelines = firelines |> 
#   filter(startdate < "2020-03-02")
# firelines$startdate[firelines$startdate < "2019-08-01"] = "2019-08-01"
# firelines = firelines |> 
#   dplyr::select(startdate) |>
#   group_by(startdate) %>%
#   summarise(do_union = FALSE) %>%
#   st_cast("MULTILINESTRING")
# 
# # setwd("E:/chapter3/roadways/")
# roads = geojson_sf("RoadSegment_EPSG4326_edit.json")
# roads$startdate = as.POSIXct(substr(roads$startdate, 1, 8), format = "%Y%m%d")
# roads = roads |> 
#   filter(startdate < "2020-03-02") |> 
#   filter(roadontype != 3)
# roads$startdate[roads$startdate < "2019-08-01"] = "2019-08-01"
# roads = roads |> 
#   dplyr::select(startdate) |>
#   group_by(startdate) %>%
#   summarise(do_union = FALSE) %>%
#   st_cast("MULTILINESTRING")
# accesslines = rbind(firelines, roads)
# st_write(accesslines, "access_lines.gpkg", delete_dsn = T)
# 
# # setwd("E:/chapter3/waterways/")
# water = geojson_sf("HydroArea_SPHERICAL_MERCATOR_edit.json")
# water$startdate = as.POSIXct(substr(water$startdate, 1, 8), format = "%Y%m%d")
# water = water |> 
#   filter(startdate < "2020-03-02") |> 
#   filter(perenniality == 1)
# water$startdate[water$startdate < "2019-08-01"] = "2019-08-01"
# water = water |> 
#   dplyr::select(startdate) |>
#   group_by(startdate) %>%
#   summarise(do_union = FALSE) %>%
#   st_cast("MULTIPOLYGON")
# st_write(water, "water.gpkg", delete_dsn = T)

# setwd("E:/chapter3/dwellings")
sf::sf_use_s2(FALSE)
houses = geojson_sf("Property_EPSG4326_edit.json")
houses$startdate = as.POSIXct(substr(houses$startdate, 1, 8), format = "%Y%m%d")
houses = houses |> 
  dplyr::select(startdate) |> 
  filter(startdate < "2020-03-02") |> filter(propertytype == 1) |> 
  st_centroid() |> 
  group_by(startdate) %>%
  summarise(do_union = FALSE) %>%
  st_cast("MULTIPOINT")
st_write(houses, "houses.gpkg", delete_dsn = T)