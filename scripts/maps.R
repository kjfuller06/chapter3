# start ####
library(sf)
library(tidyverse)
# library(UBL)
library(gam)
library(mgcv)
library(ggpubr)
library(tidymodels)
library(patchwork)
library(viridis)
library(RColorBrewer)
library(cowplot)
library(itsadug)
# library(mgcViz)
library(metR)
# library(ggh4x)
library(tmap)
library(raster)

# maps ####
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
# hill2 = raster("hillshadeformapping.tif")

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

bb = st_buffer(g[g$ID == 109123,], dist = 20000)

tmap_options(check.and.fix = T)
tmap_mode("plot")
t1 = 
  tm_shape(g, bbox = bb) + tm_fill(col = "prog", palette = turbo(20)[14:20], legend.show = FALSE) +
  tm_shape(nsw) + tm_borders()
t1
setwd("C:/chapter3/outputs")
tmap_save(t1, "firepolygons_inset.jpg", height = 2000, width = 2000, units = "px")
