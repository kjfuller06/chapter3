library(raster)
library(sf)
library(tidyverse)
library(geojsonsf)
library(rjson)
library(rgeos)

# setwd("/glade/scratch/kjfuller/data")
# # firelines ####
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
# # roads ####
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
# # water ####
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

# # distance to breaks ####
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

# further processing ####
# convert features back to simple geometries, remove Z axis and convert to projected CRS
setwd("/glade/scratch/kjfuller/data")
# setwd("E:/chapter3/waterways/")
water = st_read("water.gpkg")
water = st_transform(water, crs = targetcrs)
water = st_zm(water, drop = TRUE, what = "ZM")
st_write(water, "proj_water.gpkg", delete_dsn = T)
water = st_cast(water, "POLYGON")
water = st_transform(water, crs = targetcrs)
st_write(water, "water_polygons.gpkg", delete_dsn = T)
# setwd("E:/chapter3/roadways/")
access = st_read("access_lines.gpkg")
access = st_zm(access, drop = TRUE, what = "ZM")
access = st_cast(access, "LINESTRING")
access = st_transform(access, crs = targetcrs)
st_write(access, "access_linestrings.gpkg", delete_dsn = T)

# initial distance calculation ####
# # for each polygon, create a raster of regular cells across the surface, filter features by start date and sample the distance from each raster cell to each feature
# l = list()
# for(i in c(1:nrow(g))){
#   g_temp = g[g$ID == unique(g$ID)[i],]
#   r_temp = raster(ext = extent(g_temp), res = 10, crs = crs(g))
#   
#   water_temp = water |> 
#     filter(startdate < min(g_temp$poly_sD))
#   water_temp = as(water_temp, "Spatial")
#   
#   access_temp = access |> 
#     filter(startdate < min(g_temp$poly_sD))
#   access_temp = as(access_temp, "Spatial")
#   
#   waterMin = 
#         gDistance(access_temp, as(r_temp,"SpatialPoints"), byid=TRUE)
#   accessMin = 
#           gDistance(water_temp, as(r_temp,"SpatialPoints"), byid=TRUE)
#   ## dd is a matrix with ncol == nrow(water_temp) and nrow == ncell(r_temp)
#   ## columns correspond to polygons or lines, rows correspond to systematically chosen points in the selected polygon
#   ## calculate distance between water and raster cells
#   
#   df = cbind(waterMin, accessMin)
#   g_temp$breaksMin = 
#     median(as.numeric(
#       apply(df, 1, min)))
#   ## combine distances by cell, calculate the minimum for each row (each cell), calculate the median min distance to a break for each polygon
#   
#   st_geometry(g_temp) = NULL
#   g_temp = g_temp |> 
#     dplyr::select(ID, breaksMin)
#   l[[i]] = g_temp
# }
# l = bind_rows(l)
# write.csv(l, "isochron_breaks.csv", row.names = F)
# 
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