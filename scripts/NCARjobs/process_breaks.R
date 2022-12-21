library(raster)
library(sf)
library(tidyverse)
library(geojsonsf)
library(rjson)
library(rgeos)
library(terra)
library(FNN)

# # process isochrons ####
# setwd("/glade/scratch/kjfuller/data")
# # setwd("E:/chapter3/isochrons/Progall_ffdi_v3")
# g = st_read("progall_ffdi_v3.shp")
# targetcrs = st_crs(g)
# g$ID = c(1:nrow(g))
# g$poly_sD = as.POSIXct(g$lasttim, format = "%Y-%m-%d %H:%M")
# g$poly_eD = as.POSIXct(g$time, format = "%Y-%m-%d %H:%M")
# g$progtime = difftime(g$poly_eD, g$poly_sD, units = "hours")
# g$progtime = as.numeric(g$progtime)
# # filter polygons
# g = g |>
#   filter(progtime > 0) |>
#   filter(progtime <= 192) ## progression time < 8 days (8 * 24 = 192)
# g = g |>
#   dplyr::select(ID, poly_sD)
# st_write(g, "isochrons_8days.gpkg", delete_dsn = T)

# # firelines ####
# setwd("E:/chapter3/isochrons")
# iso = st_read("isochrons_8days.gpkg")
# targetcrs = st_crs(iso)
# iso$poly_sD = as.POSIXct(iso$poly_sD)
# iso = iso |>
#   dplyr::select(ID, poly_sD)
# ## length(unique(iso$ID)) = 106,729
# 
# setwd("E:/chapter3/fire lines/")
# firelines = geojson_sf("ClassifiedFireTrail_EPSG4326_edit.json")
# firelines$startdate = as.POSIXct(substr(firelines$startdate, 1, 8), format = "%Y%m%d")
# firelines = firelines |>
#   filter(startdate < "2020-03-02")
# firelines$startdate[firelines$startdate < "2019-08-01"] = "2019-08-01"
# firelines = firelines |>
#   dplyr::select(startdate)
# firelines = st_zm(firelines, drop = TRUE, what = "ZM")
# firelines = st_transform(firelines, crs = targetcrs)
# st_write(firelines, "firelines.gpkg", delete_dsn = T)
# 
# # to start, remove all features which are not within 10km of any fire
# iso_ext = st_as_sf(as.polygons(ext(iso)))
# st_crs(iso_ext) = targetcrs
# iso_ext = st_buffer(iso_ext, dist = 10000)
# firelines = firelines[iso_ext,]
# st_write(firelines, "firelines_restricted10km.gpkg", delete_dsn = T)
# 
# # roads ####
# # setwd("E:/chapter3/isochrons")
# # iso = st_read("isochrons_8days.gpkg")
# # targetcrs = st_crs(iso)
# # iso$poly_sD = as.POSIXct(iso$poly_sD)
# # iso = iso |>
# #   dplyr::select(ID, poly_sD)
# # ## length(unique(iso$ID)) = 106,729
# 
# setwd("E:/chapter3/roadways/")
# roads = geojson_sf("RoadSegment_EPSG4326_edit.json")
# roads$startdate = as.POSIXct(substr(roads$startdate, 1, 8), format = "%Y%m%d")
# roads = roads |>
#   filter(startdate < "2020-03-02") |>
#   filter(roadontype != 3)
# roads$startdate[roads$startdate < "2019-08-01"] = "2019-08-01"
# roads = roads |>
#   dplyr::select(startdate)
# roads = st_zm(roads, drop = TRUE, what = "ZM")
# roads = st_transform(roads, crs = targetcrs)
# st_write(roads, "roads.gpkg", delete_dsn = T)
# 
# # to start, remove all features which are not within 10km of any fire
# # iso_ext = st_as_sf(as.polygons(ext(iso)))
# # st_crs(iso_ext) = targetcrs
# # iso_ext = st_buffer(iso_ext, dist = 10000)
# roads = roads[iso_ext,]
# st_write(roads, "roads_restricted10km.gpkg", delete_dsn = T)
# 
# # water ####
# # setwd("E:/chapter3/isochrons")
# # iso = st_read("isochrons_8days.gpkg")
# # targetcrs = st_crs(iso)
# # iso$poly_sD = as.POSIXct(iso$poly_sD)
# # iso = iso |>
# #   dplyr::select(ID, poly_sD)
# # ## length(unique(iso$ID)) = 106,729
# 
# setwd("E:/chapter3/waterways/")
# water = geojson_sf("HydroArea_SPHERICAL_MERCATOR_edit.json")
# water$startdate = as.POSIXct(substr(water$startdate, 1, 8), format = "%Y%m%d")
# water = water |>
#   filter(startdate < "2020-03-02") |>
#   filter(perenniality == 1)
# water$startdate[water$startdate < "2019-08-01"] = "2019-08-01"
# water = water |>
#   dplyr::select(startdate)
# water = st_zm(water, drop = TRUE, what = "ZM")
# water = st_transform(water, crs = targetcrs)
# st_write(water, "water.gpkg", delete_dsn = T)
# 
# # to start, remove all features which are not within 10km of any fire
# # iso_ext = st_as_sf(as.polygons(ext(iso)))
# # st_crs(iso_ext) = targetcrs
# # iso_ext = st_buffer(iso_ext, dist = 10000)
# water = water[iso_ext,]
# st_write(water, "water_restricted10km.gpkg", delete_dsn = T)

# # housing density ####
# # setwd("E:/chapter3/isochrons/Progall_ffdi_v3")
# g = st_read("progall_ffdi_v3.shp")
# targetcrs = st_crs(g)
# g$ID = c(1:nrow(g))
# g$poly_sD = as.POSIXct(g$lasttim, format = "%Y-%m-%d %H:%M")
# g$poly_eD = as.POSIXct(g$time, format = "%Y-%m-%d %H:%M")
# g$progtime = difftime(g$poly_eD, g$poly_sD, units = "hours")
# g$progtime = as.numeric(g$progtime)
# g = g |>
#   filter(progtime > 0) |>
#   filter(progtime <= 192) ## progression time < 8 days (8 * 24 = 192)
# g = g |>
#   dplyr::select(ID, poly_sD, area)
# 
# # # setwd("E:/chapter3/dwellings")
# # sf::sf_use_s2(FALSE)
# # houses = geojson_sf("Property_EPSG4326_edit.json")
# # houses$startdate = as.POSIXct(substr(houses$startdate, 1, 8), format = "%Y%m%d")
# # houses = houses |>
# #   filter(startdate < "2020-03-02") |> filter(propertytype == 1) |>
# #   dplyr::select(startdate) |>
# #   st_transform(crs = targetcrs) |>
# #   st_centroid()
# # st_write(houses, "houses.gpkg", delete_dsn = T)
# 
# houses = st_read("houses.gpkg")
# houses$startdate = as.POSIXct(houses$startdate, 1, 8)
# sf::sf_use_s2(FALSE)
# 
# houses = st_intersection(houses, g)
# st_write(houses, "houses_isochrons.gpkg", delete_dsn = T)
# 
# st_geometry(houses) = NULL
# houses = houses |>
#   filter(startdate < poly_sD)
# 
# house_agg = houses |>
#   group_by(ID) |>
#   tally() |>
#   as.data.frame()
# house_id = houses |>
#   dplyr::select(ID, area) |>
#   unique()
# houses = full_join(house_agg, house_id)
# houses$house.density = houses$n/houses$area
# write.csv(houses, "housing_density.csv", row.names = F)