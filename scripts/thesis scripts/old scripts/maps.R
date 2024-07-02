library(tidyverse)
library(tmap)
library(sf)
library(raster)
library(viridis)

# maps ####
# distance to water ####
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

# isochrons ####
setwd("D:/chapter1/data")
nsw = st_read("NSW_sans_islands.shp")
nsw_buff = st_buffer(nsw, dist = 50000)
aus = readRDS("gadm36_AUS_1_sp.rds")
setwd("E:/chapter3/for GAMs")
# g = st_read(paste0("ch3_forGAMs_poly_prefire", x, "_final.gpkg"))
g = st_read(paste0("ch3_forGAMs_poly_prefire180_final_redo_2-15.gpkg"))
setwd("D:/chapter1/other_data/Final/terrain variables")
# hill2 = raster("proj_dem_hillshade_30m.tif")
# hill2 = aggregate(hill2, fact = 10)
# hillproj = projectRaster(hill2, crs = "EPSG:4326")
# hillproj = mask(hillproj, nsw)
# writeRaster(hillproj, "hillshadeformapping.tif", overWrite = T)
hill2 = raster("hillshadeformapping.tif")

bb = st_buffer(g, dist = 50000)

tmap_options(check.and.fix = T)
tmap_mode("plot")
t1 =
  tm_shape(aus, bbox = bb) +
  tm_fill(col = gray(75/100)) +
  tm_shape(hill2) +
  tm_raster(palette = gray(50:100 / 100), n = 100, legend.show = FALSE) +
  # tm_shape(nsw) +
  # tm_borders() +
  tm_shape(g) + tm_fill(col = "prog", palette = turbo(20)[14:20], legend.show = FALSE) +
  tm_graticules(lines = F) + 
  tm_layout(inner.margins = 0)
# t1

setwd("C:/chapter3/outputs")
# tmap_save(t1, "firepolygons.jpg", height = 3000, width = 2000, units = "px")
tmap_save(t1, "firepolygons_redo_2-15.jpg", height = 3000, width = 2000, units = "px")

t1 =
  tm_shape(g) + tm_fill(col = "prog", palette = turbo(20)[14:20])
t1

setwd("C:/chapter3/outputs")
tmap_save(t1, "polygons_legend.jpg", height = 3000, width = 2000, units = "px")

# t1 =
tm_shape(aus, bbox = bb) +
  tm_fill(col = gray(75/100)) +
  tm_shape(hill2) +
  tm_raster(palette = gray(50:100 / 100), n = 100, legend.show = FALSE) +
  # tm_shape(nsw) +
  # tm_borders() +
  tm_shape(g) + tm_fill(col = "wind.stdev", palette = turbo(20)[14:20], legend.show = FALSE) +
  tm_graticules(lines = F) + 
  tm_layout(inner.margins = 0)
# t1

# fire polygons (redo) ####
setwd("D:/chapter1/data")
nsw = st_read("NSW_sans_islands.shp")
nsw_buff = st_buffer(nsw, dist = 50000)
aus = readRDS("gadm36_AUS_1_sp.rds")
setwd("E:/chapter3/for GAMs")
g = st_read(paste0("ch3_forGAMs_poly_prefire180_final_redo_2-15.gpkg"))
# setwd("D:/chapter1/other_data/Final/terrain variables")
# hill2 = raster("proj_dem_hillshade_30m.tif")
# hill2 = aggregate(hill2, fact = 10)
# hillproj = projectRaster(hill2, crs = "EPSG:4326")
# hillproj = mask(hillproj, nsw)
# writeRaster(hillproj, "hillshadeformapping.tif", overWrite = T)
hill2 = raster("hillshadeformapping.tif")

bb = st_buffer(g, dist = 50000)

tmap_options(check.and.fix = T)
tmap_mode("plot")
t1 =
  tm_shape(aus, bbox = bb) +
  tm_fill(col = gray(75/100)) +
  # tm_shape(hill2) +
  tm_raster(palette = gray(50:100 / 100), n = 100, legend.show = FALSE) +
  # tm_shape(nsw) +
  # tm_borders() +
  tm_shape(g) + tm_fill(col = "prog", palette = turbo(20)[14:20], legend.show = FALSE) +
  tm_graticules(lines = F) + 
  tm_layout(inner.margins = 0)
t1

setwd("D:/chapter3/outputs/GAMs")
# tmap_save(t1, "firepolygons.jpg", height = 3000, width = 2000, units = "px")
tmap_save(t1, "firepolygons_redo_1-27.jpg", height = 3000, width = 2000, units = "px")

t1 =
  tm_shape(g) + tm_fill(col = "prog", palette = turbo(20)[14:20])
t1

setwd("D:/chapter3/outputs/GAMs")
tmap_save(t1, "polygons_legend.jpg", height = 3000, width = 2000, units = "px")

t1 =
  tm_shape(aus, bbox = bb) +
  tm_fill(col = gray(75/100)) +
  tm_shape(hill2) +
  tm_raster(palette = gray(50:100 / 100), n = 100, legend.show = FALSE) +
  # tm_shape(nsw) +
  # tm_borders() +
  tm_shape(g) + tm_fill(col = "wind.stdev", palette = turbo(20)[14:20], legend.show = FALSE) +
  tm_graticules(lines = F) + 
  tm_layout(inner.margins = 0)
# t1

IDs = c(109832, 109785, 109660, 109595, 109447, 109521, 109832, 109841, 109845, 109701, 109510, 109874)
g.box = g |> 
  filter(ID %in% IDs) |> dplyr::select(ID, poly_sD, poly_eD)
touching_list = st_touches(g.box)
firelines = list()
for(i in c(1:nrow(g.box))){
  touch.temp = g.box[touching_list[[i]],]
  # touch.temp = touch.temp |> filter(poly_eD == g.box$poly_sD[i])
  fire.temp = st_cast(st_intersection(g.box[i,], touch.temp))
  fire.temp = st_cast(fire.temp)[which(st_is(st_cast(fire.temp), c("LINESTRING", "MULTILINESTRING"))),]
  firelines[[i]] = fire.temp
}
firelines = bind_rows(firelines)

tmap_options(check.and.fix = T)
tmap_mode("view")
t1 = 
  # tm_shape(g, bbox = bb) + tm_fill(col = "prog", palette = turbo(20)[14:20], legend.show = FALSE) +
  tm_shape(g.box) + tm_fill(col = "poly_sD", palette = rev(colorRampPalette(colors = c("grey20", "grey"))(8))) +
  tm_shape(firelines) + tm_lines() +
  tm_shape(nsw) + tm_borders()
t1
setwd("C:/chapter3/outputs")
tmap_save(t1, "firepolygons_inset_legend.jpg", height = 2000, width = 2000, units = "px")

t1 = 
  tm_shape(g.box) + tm_fill(col = "poly_sD", palette = rev(colorRampPalette(colors = c("grey20", "grey"))(8)), legend.show = F) +
  tm_shape(firelines) + tm_lines() +
  tm_shape(nsw) + tm_borders()
# t1
setwd("C:/chapter3/outputs")
tmap_save(t1, "firepolygons_inset.jpg", height = 2000, width = 2000, units = "px")
