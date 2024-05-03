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
