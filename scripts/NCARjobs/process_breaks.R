library(raster)
library(sf)
library(tidyverse)
library(geojsonsf)
library(rjson)
library(rgeos)
library(terra)
library(FNN)

# process isochrons ####
setwd("/glade/scratch/kjfuller/data")
setwd("E:/chapter3/isochrons/Progall_ffdi_v3")
g = st_read("progall_ffdi_v3.shp")
targetcrs = st_crs(g)
g$ID = c(1:nrow(g))
g$poly_sD = as.POSIXct(g$lasttim, format = "%Y-%m-%d %H:%M")
g$poly_eD = as.POSIXct(g$time, format = "%Y-%m-%d %H:%M")
g$progtime = difftime(g$poly_eD, g$poly_sD, units = "hours")
g$progtime = as.numeric(g$progtime)
# filter polygons
g = g |>
  filter(progtime > 0) |>
  filter(progtime <= 192) ## progression time < 8 days (8 * 24 = 192)
g = g |>
  dplyr::select(ID, poly_sD, poly_eD, progtime)
setwd("E:/chapter3/isochrons")
st_write(g, "isochrons_8days.gpkg", delete_dsn = T)

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
# roads ####
# setwd("E:/chapter3/isochrons")
# iso = st_read("isochrons_8days.gpkg")
# targetcrs = st_crs(iso)
# iso$poly_sD = as.POSIXct(iso$poly_sD)
# iso = iso |>
#   dplyr::select(ID, poly_sD)
# ## length(unique(iso$ID)) = 106,729

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

setwd("E:/chapter3/roadways/")
roads = geojson_sf("RoadNameExtent_EPSG4326_edit.json")
roads$startdate = as.POSIXct(substr(roads$startdate, 1, 8), format = "%Y%m%d")
roads = roads |>
  filter(startdate < "2020-03-02") |>
  filter(functionhierarchy != 9 & functionhierarchy != 8) |> 
  filter(operationalstatus == 1) |> 
  filter(relevance == 1)
## not a walking path, not unimproved road (which includes driveways)
## operational
## cut out non-significant roadways
roads$startdate[roads$startdate < "2019-08-01"] = "2019-08-01"
roads = st_zm(roads, drop = TRUE, what = "ZM")
roads = st_transform(roads, crs = targetcrs)
st_write(roads, "roads.gpkg", delete_dsn = T)

# to start, remove all features which are not within 10km of any fire
# iso_ext = st_as_sf(as.polygons(ext(iso)))
# st_crs(iso_ext) = targetcrs
# iso_ext = st_buffer(iso_ext, dist = 10000)
roads = roads[iso_ext,]
st_write(roads, "roads_restricted10km.gpkg", delete_dsn = T)

# water ####
setwd("E:/chapter3/isochrons")
iso = st_read("isochrons_8days.gpkg")
targetcrs = st_crs(iso)
iso$poly_sD = as.POSIXct(iso$poly_sD)
iso = iso |>
  dplyr::select(ID, poly_sD)
## length(unique(iso$ID)) = 106,729

# setwd("E:/chapter3/waterways/")
# water2 = geojson_sf("HydroArea_SPHERICAL_MERCATOR_edit.json")
# ## classsubtype 1 == water body
# ## classsubtype 2 == water course
# 
# water2$startdate = as.POSIXct(substr(water2$startdate, 1, 8), format = "%Y%m%d")
# water2 = water2 |>
#   filter(startdate < "2020-03-02") |>
#   filter(perenniality == 1)
# water2$startdate[water2$startdate < "2019-08-01"] = "2019-08-01"
# # water = water |>
# #   dplyr::select(startdate)
# water2 = st_zm(water2, drop = TRUE, what = "ZM")
# ## all features represented as polygons or multipolygons- can filter by Shape__Area to get the most major water features
# water2 = water2 |> 
#   filter(Shape__Area > 10000)
# ## no good- still small features showing up and level of detail is still patchy

setwd("E:/chapter3/waterways/")
water = geojson_sf("NamedWatercourse_EPSG4326_edit.json")
## relevance is a good control here- the higher the number, the smaller the feature

water$startdate = as.POSIXct(substr(water$startdate, 1, 8), format = "%Y%m%d")
water = water |>
  filter(startdate < "2020-03-02")
water$startdate[water$startdate < "2019-08-01"] = "2019-08-01"
water = st_zm(water, drop = TRUE, what = "ZM")

par(mfrow = c(3, 3))
plot(st_geometry(water |> filter(relevance <= 1)), main = "relevance <= 1")
plot(st_geometry(water |> filter(relevance <= 2)), main = "relevance <= 2")
plot(st_geometry(water |> filter(relevance <= 3)), main = "relevance <= 3")
plot(st_geometry(water |> filter(relevance <= 4)), main = "relevance <= 4")
plot(st_geometry(water |> filter(relevance <= 5)), main = "relevance <= 5")
plot(st_geometry(water |> filter(relevance <= 6)), main = "relevance <= 6")
plot(st_geometry(water |> filter(relevance <= 7)), main = "relevance <= 7")
plot(st_geometry(water |> filter(relevance <= 8)), main = "relevance <= 8")
plot(st_geometry(water |> filter(relevance <= 9)), main = "relevance <= 9")

water = st_transform(water, crs = targetcrs)
st_write(water, "water.gpkg", delete_dsn = T)

# to start, remove all features which are not within 10km of any fire
iso_ext = st_as_sf(as.polygons(ext(iso)))
st_crs(iso_ext) = targetcrs
iso_ext = st_buffer(iso_ext, dist = 10000)
water = water[iso_ext,]
st_write(water, "water_restricted10km.gpkg", delete_dsn = T)

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

# maps ####
setwd("E:/chapter3/waterways")
water = st_read("water.gpkg")
water1 = water |> 
  filter(relevance <= 1)
water2 = water |> 
  filter(relevance <= 2)
water3 = water |> 
  filter(relevance <= 3)
water4 = water |> 
  filter(relevance <= 4)
setwd("D:/chapter1/bark-type-SDM/data")
nsw = st_read("NSW_sans_islands.shp")
nsw_buff = st_buffer(nsw, dist = 50000)
aus = readRDS("gadm36_AUS_1_sp.rds")
setwd("D:/chapter1/other_data/Final/terrain variables")
hill2 = raster("hillshadeformapping.tif")

tmap_options(check.and.fix = T)
tmap_mode("plot")
t1 =
tm_shape(aus, bbox = nsw) +
  tm_fill(col = gray(75/100)) +
  tm_shape(hill2) +
  tm_raster(palette = gray(25:100 / 100), n = 100, legend.show = FALSE) +
  # tm_shape(nsw) +
  # tm_borders() +
  tm_shape(water4) + tm_lines(col = mako(10)[6], lwd = 0.25) +
  tm_shape(water3) + tm_lines(col = mako(10)[6], lwd = 0.75) +
  tm_shape(water2) + tm_lines(col = mako(10)[6], lwd = 1) +
  tm_shape(water1) + tm_lines(col = mako(10)[6], lwd = 1.5) +
  tm_graticules(lines = F)
t1

setwd("D:/chapter3/outputs/GAMs")
tmap_save(t1, "water_orders.jpg", height = 3000, width = 3000, units = "px")

t1 =
  tm_shape(water1, bbox = nsw_buff) + tm_lines(col = mako(10)[6], lwd = 0.5) +
  tm_add_legend(type = "line", 
                labels = c("First order", "Second order", "Third order", "Fourth order"),
                col = mako(10)[6],
                title = "Significance of\nwater features",
                lwd = c(0.25, 0.75, 1, 1.5))
t1

setwd("D:/chapter3/outputs/GAMs")
tmap_save(t1, "water_legend.jpg", height = 2000, width = 2000, units = "px")
