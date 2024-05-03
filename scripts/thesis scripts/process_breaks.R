library(raster)
library(sf)
library(tidyverse)
library(geojsonsf)
library(rjson)
library(rgeos)
library(terra)
library(FNN)
library(tmap)
library(viridis)

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

# firelines ####
setwd("E:/chapter3/isochrons")
iso = st_read("isochrons_8days.gpkg")
targetcrs = st_crs(iso)
iso$poly_sD = as.POSIXct(iso$poly_sD)
iso = iso |>
  dplyr::select(ID, poly_sD)
## length(unique(iso$ID)) = 106,729

setwd("E:/chapter3/fire lines/")
firelines = geojson_sf("ClassifiedFireTrail_EPSG4326_edit.json")
firelines$startdate = as.POSIXct(substr(firelines$startdate, 1, 8), format = "%Y%m%d")
firelines = firelines |>
  filter(startdate < "2020-03-02")
firelines$startdate[firelines$startdate < "2019-08-01"] = "2019-08-01"
firelines = firelines |>
  dplyr::select(startdate)
firelines = st_zm(firelines, drop = TRUE, what = "ZM")
firelines = st_transform(firelines, crs = targetcrs)
st_write(firelines, "firelines.gpkg", delete_dsn = T)

# to start, remove all features which are not within 10km of any fire
iso_ext = st_as_sf(as.polygons(ext(iso)))
st_crs(iso_ext) = targetcrs
iso_ext = st_buffer(iso_ext, dist = 10000)
firelines = firelines[iso_ext,]
st_write(firelines, "firelines_restricted10km.gpkg", delete_dsn = T)

# roads ####
setwd("E:/chapter3/isochrons")
iso = st_read("isochrons_8days.gpkg")
targetcrs = st_crs(iso)
iso$poly_sD = as.POSIXct(iso$poly_sD)
iso = iso |>
  dplyr::select(ID, poly_sD)
## length(unique(iso$ID)) = 106,729

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
# setwd("E:/chapter3/isochrons")
# iso = st_read("isochrons_8days.gpkg")
# targetcrs = st_crs(iso)
# iso$poly_sD = as.POSIXct(iso$poly_sD)
# iso = iso |>
#   dplyr::select(ID, poly_sD)
# ## length(unique(iso$ID)) = 106,729
# 
# setwd("E:/chapter3/waterways/")
# water = geojson_sf("HydroLine_EPSG4326_edit.json")
# ## relevance is a good control here- the higher the number, the smaller the feature
# 
# water$startdate = as.POSIXct(substr(water$startdate, 1, 8), format = "%Y%m%d")
# water = water |>
#   filter(startdate < "2020-03-02")
# nrow(water)
# ## 2,697,286
# water = water |> 
#   filter(classsubtype == 1)
# nrow(water)
# ## 2,686,517
# water = water |> 
#   filter(hydrotype == 1)
# nrow(water)
# ## 2,612,993
# water$startdate[water$startdate < "2019-08-01"] = "2019-08-01"
# water = st_zm(water, drop = TRUE, what = "ZM")
# 
# water = st_transform(water, crs = targetcrs)
# st_write(water, "hydro_nonpere.gpkg", delete_dsn = T)
# 
# # to start, remove all features which are not within 10km of any fire
# iso_ext = st_as_sf(as.polygons(ext(iso)))
# st_crs(iso_ext) = targetcrs
# iso_ext = st_buffer(iso_ext, dist = 10000)
# water = water[iso_ext,]
# st_write(water, "hydro_nonpere_restricted10km.gpkg", delete_dsn = T)

setwd("D:/chapter3/climatedem")
r = raster("strahler2_thin.tif")
df = matrix(c(0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
              2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
              2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
            ncol = 3, byrow = F)
r = reclassify(r, rcl = df)
writeRaster(r, "strahler2_reclass.tif", overwrite = T)

r = raster("strahler2_reclass.tif")
df = matrix(c(0, 11,
              11, 12,
              NA, 1),
            ncol = 3, byrow = F)
r12 = reclassify(r, rcl = df)
r12
writeRaster(r12, "strahler2_reclass12.tif", overwrite = T)
df = matrix(c(0, 10, 11,
              10, 11, 12,
              NA, 1, NA),
            ncol = 3, byrow = F)
r11 = reclassify(r, rcl = df)
r11
# plot(r11)
writeRaster(r11, "strahler2_reclass11.tif", overwrite = T)
df = matrix(c(0, 9, 10,
              9, 10, 12,
              NA, 1, NA),
            ncol = 3, byrow = F)
r10 = reclassify(r, rcl = df)
r10
writeRaster(r10, "strahler2_reclass10.tif", overwrite = T)
df = matrix(c(0, 8, 9,
              8, 9, 12,
              NA, 1, NA),
            ncol = 3, byrow = F)
r9 = reclassify(r, rcl = df)
r9
writeRaster(r9, "strahler2_reclass9.tif", overwrite = T)
df = matrix(c(0, 7, 8,
              7, 8, 12,
              NA, 1, NA),
            ncol = 3, byrow = F)
r8 = reclassify(r, rcl = df)
r8
writeRaster(r8, "strahler2_reclass8.tif", overwrite = T)
df = matrix(c(0, 6, 7,
              6, 7, 12,
              NA, 1, NA),
            ncol = 3, byrow = F)
r7 = reclassify(r, rcl = df)
r7
writeRaster(r7, "strahler2_reclass7.tif", overwrite = T)
df = matrix(c(0, 5, 6,
              5, 6, 12,
              NA, 1, NA),
            ncol = 3, byrow = F)
r6 = reclassify(r, rcl = df)
r6
writeRaster(r6, "strahler2_reclass6.tif", overwrite = T)
df = matrix(c(0, 4, 5,
              4, 5, 12,
              NA, 1, NA),
            ncol = 3, byrow = F)
r5 = reclassify(r, rcl = df)
r5
writeRaster(r5, "strahler2_reclass5.tif", overwrite = T)
df = matrix(c(0, 3, 4,
              3, 4, 12,
              NA, 1, NA),
            ncol = 3, byrow = F)
r4 = reclassify(r, rcl = df)
r4
writeRaster(r4, "strahler2_reclass4.tif", overwrite = T)
df = matrix(c(0, 2, 3,
              2, 3, 12,
              NA, 1, NA),
            ncol = 3, byrow = F)
r3 = reclassify(r, rcl = df)
r3
writeRaster(r3, "strahler2_reclass3.tif", overwrite = T)
df = matrix(c(0, 2,
              2, 12,
              1, NA),
            ncol = 3, byrow = F)
r2 = reclassify(r, rcl = df)
r2
writeRaster(r2, "strahler2_reclass2.tif", overwrite = T)

# maps ####
setwd("E:/chapter3/waterways")
water = st_read("hydro_nonpere.gpkg")
water1 = water |> 
  filter(relevance <= 3)
water2 = water |> 
  filter(relevance <= 4)
water3 = water |> 
  filter(relevance <= 5)
water4 = water |> 
  filter(relevance <= 6)
setwd("D:/chapter1/bark-type-SDM/data")
nsw = st_read("NSW_sans_islands.shp")
nsw_buff = st_buffer(nsw, dist = 50000)
aus = readRDS("gadm36_AUS_1_sp.rds")
setwd("D:/chapter1/other_data/Final/terrain variables")
hill2 = raster("hillshadeformapping.tif")
setwd("E:/chapter3/for GAMs")
# g = st_read(paste0("ch3_forGAMs_poly_prefire", x, "_final.gpkg"))
g = st_read(paste0("ch3_forGAMs_poly_prefire180_final_redo.gpkg"))
bb = st_buffer(g, dist = 50000)

tmap_options(check.and.fix = T)
tmap_mode("plot")
t1 =
  tm_shape(water1, bbox = bb) + tm_lines(col = mako(10)[6], lwd = 0.5) +
  tm_add_legend(type = "line", 
                labels = c("Third order", "Fourth order", "Fifth order", "Sixth order"),
                col = mako(10)[6],
                title = "Classification of\nwater features",
                lwd = c(1.5, 1.0, 0.75, 0.25)) +
  tm_scale_bar()
setwd("D:/chapter3/outputs/GAMs")
tmap_save(t1, "hydro_legend.jpg", height = 2000, width = 2000, units = "px")

t1 =
  tm_shape(aus, bbox = bb) +
  tm_fill(col = gray(75/100)) +
  tm_shape(hill2) +
  tm_raster(palette = gray(25:100 / 100), n = 100, legend.show = FALSE) +
  # tm_shape(nsw) +
  # tm_borders() +
  tm_shape(water4) + tm_lines(col = mako(10)[6], lwd = 0.25) +
  tm_shape(water3) + tm_lines(col = mako(10)[6], lwd = 0.75) +
  tm_shape(water2) + tm_lines(col = mako(10)[6], lwd = 1) +
  tm_shape(water1) + tm_lines(col = mako(10)[6], lwd = 1.5) +
  tm_graticules(lines = F) + 
  tm_layout(inner.margins = 0)
t1

setwd("D:/chapter3/outputs/GAMs")
tmap_save(t1, "hydro_nonpere_orders.jpg", height = 3000, width = 2000, units = "px")

setwd("E:/chapter3/waterways")
water = st_read("hydro.gpkg")
water1 = water |> 
  filter(relevance <= 3)
water2 = water |> 
  filter(relevance <= 4)
water3 = water |> 
  filter(relevance <= 5)
water4 = water |> 
  filter(relevance <= 6)
setwd("D:/chapter1/bark-type-SDM/data")
nsw = st_read("NSW_sans_islands.shp")
nsw_buff = st_buffer(nsw, dist = 50000)
aus = readRDS("gadm36_AUS_1_sp.rds")
setwd("D:/chapter1/other_data/Final/terrain variables")
hill2 = raster("hillshadeformapping.tif")
setwd("E:/chapter3/for GAMs")
# g = st_read(paste0("ch3_forGAMs_poly_prefire", x, "_final.gpkg"))
g = st_read(paste0("ch3_forGAMs_poly_prefire180_final_redo.gpkg"))
bb = st_buffer(g, dist = 50000)

tmap_options(check.and.fix = T)
tmap_mode("plot")
t1 =
  tm_shape(aus, bbox = bb) +
  tm_fill(col = gray(75/100)) +
  tm_shape(hill2) +
  tm_raster(palette = gray(25:100 / 100), n = 100, legend.show = FALSE) +
  # tm_shape(nsw) +
  # tm_borders() +
  tm_shape(water4) + tm_lines(col = mako(10)[6], lwd = 0.25) +
  tm_shape(water3) + tm_lines(col = mako(10)[6], lwd = 0.75) +
  tm_shape(water2) + tm_lines(col = mako(10)[6], lwd = 1) +
  tm_shape(water1) + tm_lines(col = mako(10)[6], lwd = 1.5) +
  tm_graticules(lines = F) + 
  tm_layout(inner.margins = 0)
t1

setwd("D:/chapter3/outputs/GAMs")
tmap_save(t1, "hydro_orders.jpg", height = 3000, width = 2000, units = "px")

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
setwd("E:/chapter3/for GAMs")
# g = st_read(paste0("ch3_forGAMs_poly_prefire", x, "_final.gpkg"))
g = st_read(paste0("ch3_forGAMs_poly_prefire", x, "_final_redo.gpkg"))
bb = st_buffer(g, dist = 50000)

tmap_options(check.and.fix = T)
tmap_mode("plot")
t1 =
tm_shape(aus, bbox = bb) +
  tm_fill(col = gray(75/100)) +
  tm_shape(hill2) +
  tm_raster(palette = gray(25:100 / 100), n = 100, legend.show = FALSE) +
  # tm_shape(nsw) +
  # tm_borders() +
  tm_shape(water4) + tm_lines(col = mako(10)[6], lwd = 0.25) +
  tm_shape(water3) + tm_lines(col = mako(10)[6], lwd = 0.75) +
  tm_shape(water2) + tm_lines(col = mako(10)[6], lwd = 1) +
  tm_shape(water1) + tm_lines(col = mako(10)[6], lwd = 1.5) +
  tm_graticules(lines = F) + 
  tm_layout(inner.margins = 0)
t1

setwd("D:/chapter3/outputs/GAMs")
tmap_save(t1, "water_orders.jpg", height = 3000, width = 2000, units = "px")

t1 =
  tm_shape(water1, bbox = bb) + tm_lines(col = mako(10)[6], lwd = 0.5) +
  tm_add_legend(type = "line", 
                labels = c("First order", "Second order", "Third order", "Fourth order"),
                col = mako(10)[6],
                title = "Classification of\nwater features",
                lwd = c(1.5, 1.0, 0.75, 0.25)) +
  tm_scale_bar()
t1

setwd("D:/chapter3/outputs/GAMs")
tmap_save(t1, "water_legend.jpg", height = 2000, width = 2000, units = "px")
