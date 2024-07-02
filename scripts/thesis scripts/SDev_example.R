library(tidyverse)
library(sf)
library(raster)
library(exactextractr)

tmap_mode("view")
setwd("D:/chapter1/data")
dem = raster("proj_dem_s.tif")
xmin = extent(dem)[1]
xmax = extent(dem)[2]
ymin = extent(dem)[3]
ymax = extent(dem)[4]

xs = seq(xmin, xmax, length.out = 12)[2:11]
ys = seq(ymin, ymax, length.out = 12)[2:11]
cords = expand.grid(x = xs,
                    y = ys)
cords = st_as_sf(cords, coords = c("x", "y"), crs = st_crs(dem))
cords$ID = c(1:nrow(cords))
cords = st_buffer(cords, dist = 1000)
cords5 = st_buffer(cords, dist = 5000)
cords10 = st_buffer(cords, dist = 10000)
# tm_shape(cords10) + tm_polygons() +
#   tm_shape(cords5) + tm_polygons() +
#   tm_shape(cords) + tm_dots()

elev_sd = raster::extract(dem, cords, method = 'simple')
cords$elev_sd = lapply(elev_sd, FUN = sd, na.rm = T)
cords$distance = 1000

elev_sd = raster::extract(dem, cords5, method = 'simple')
cords5$elev_sd = lapply(elev_sd, FUN = sd, na.rm = T)
cords5$distance = 5000

elev_sd = raster::extract(dem, cords10, method = 'simple')
cords10$elev_sd = lapply(elev_sd, FUN = sd, na.rm = T)
cords10$distance = 10000

dfall = rbind(cords, cords5, cords10)
backup = dfall
dfall$elev_sd = unlist(dfall$elev_sd)
summary(dfall)
head(dfall)

st_geometry(dfall) = NULL

g1 = ggplot(data = dfall, aes(y = elev_sd, group = distance)) +
  geom_boxplot(aes(col = distance)) +
  xlab("Sample ID") +
  ylab("Standard deviation in elevation (m)")
g1
setwd("C:/chapter3/outputs")
ggsave("SDev_example.jpg", g1, device = "jpeg", height = 5, width = 5)
