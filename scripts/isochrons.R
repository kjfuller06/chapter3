library(sf)
library(tidyverse)
library(tmap)

setwd("D:/chapter1/bark-type-SDM/data")
nsw = st_read("NSW_sans_islands.shp")

setwd("E:/chapter3/isochrons")
isochrons = list.files()
isochrons = isochrons[!grepl(".zip", isochrons)]
isochrons

# setwd("E:/chapter3/isochrons/badja/badja") ## included in p1
# shp = list.files(pattern = ".shp$")
# b1 = st_read(shp[2])
# b1
# min(b1$TIME)
# max(b1$TIME)
# ## Dec 2019 to Feb 2020

# tm_shape(b1) + tm_polygons()

# setwd(paste0("E:/chapter3/isochrons/", isochrons[2])) ## included in p1
# shp = list.files(pattern = ".shp$")
# c1 = st_read(shp[1])
# c1
# c1$datetime = as.POSIXct(c1$datetime, format = "%Y-%m-%d %H:%M", tz = "UTC")
# min(c1$datetime, na.rm = TRUE)
# max(c1$datetime, na.rm = TRUE)
# ## Oct 2019 to Jan 2020

# tm_shape(c1) + tm_polygons()

# setwd(paste0("E:/chapter3/isochrons/", isochrons[3])) ## included in p1
# shp = list.files(pattern = ".shp$")
# n1 = st_read(shp[1])
# n1
# min(n1$datetime, na.rm = TRUE)
# max(n1$datetime, na.rm = TRUE)
# ## Aug 2019 to Jan 2020
# n1 = n1 %>% 
#   dplyr::select(geometry)
# 
# tmap_mode("plot")
# tm_shape(n1) + tm_polygons() + tm_shape(nsw) + tm_borders()

setwd(paste0("E:/chapter3/isochrons/", isochrons[4])) ## great metadata made by Owen- I believe this is the final product with FFDI values for all fires
shp = list.files(pattern = ".shp$")
p1 = st_read(shp[1])
p1
p1$time = as.POSIXct(p1$time, tz = "UTC")
p1$lasttim = as.POSIXct(p1$lasttim, tz = "UTC")
min(p1$lasttim, na.rm = TRUE)
max(p1$time, na.rm = TRUE)
## Aug 2019 to Mar 2020

# bound = st_bbox(n1) %>% 
#   st_as_sfc()
# tm_shape(p1, bbox = bound) + tm_polygons() + tm_shape(nsw) + tm_borders()

# setwd(paste0("E:/chapter3/isochrons/", isochrons[5], "/", isochrons[5])) ## included in p1
# shp = list.files(pattern = ".shp$")
# p2 = st_read(shp[1])
# p2
# p1$time = as.POSIXct(p1$time, tz = "UTC")
# p1$lasttim = as.POSIXct(p1$lasttim, tz = "UTC")
# min(p2$datetime, na.rm = TRUE)
# max(p2$datetime, na.rm = TRUE)
# ## Nov 2019 to Mar 2020

# tm_shape(p2) + tm_polygons()

tmap_mode("plot")
t1 = tm_shape(p1) + tm_polygons(col = "ffdi_final", border.alpha = 0, breaks = c(0, 12, 25, 50, 75, 100, 300), palette = c("green", "blue", "yellow", "orange", "red", "black")) +
  tm_shape(nsw) + tm_borders()
  # tm_shape(b1) + tm_polygons()
  # tm_shape(c1) + tm_polygons()
  # tm_shape(n1) + tm_polygons()
  # tm_shape(p2) + tm_polygons()
tmap_save(t1, height = 7, filename = "Isochrons_FFDI.jpeg")
