library(sf)
library(raster)
library(tidyverse)
library(exactextractr)
library(rgeos)

# load isochrons
setwd("/glade/scratch/kjfuller/data/chapter3")
# setwd("E:/chapter3/for GAMs")
g = st_read("ch3_forGAMs_poly_prefire180_structure.gpkg")
targetcrs = st_crs(g)
g = g |>
  dplyr::select(ID)

setwd("/glade/scratch/kjfuller/data")
# setwd("D:/chapter1/bark-type-SDM/outputs/RFs/final models/final geotiffs")
# r = raster("NSW_stringybark_distribution.tif")
r = raster("predmosaic_RF5_stringybark_07_allfires.tif")
threshold = 0.6 ## values are inverted for stringybark
# values(rpred)[values(rpred) > threshold] = 0
# values(rpred)[values(rpred) <= threshold] = 1
rcl = matrix(c(-Inf, 0.4,
               0.4, Inf,
               0, 1), 
             ncol = 3, byrow = F)
r = reclassify(x = r, rcl = rcl, right = F, include.lowest = T)
writeRaster(r, "RF5_stringybark_classified.tif", overwrite = T)

g = st_transform(g, crs = st_crs(r))
g$stringybark = raster::extract(r, g, method = 'simple', fun = median, na.rm = T)

setwd("/glade/scratch/kjfuller/data/chapter3")
g = st_transform(g, crs = targetcrs)
st_write(g, paste0("ch3_forGAMs_poly_prefire180_bark1.gpkg"), delete_dsn = T)

# load isochrons ####
setwd("/glade/scratch/kjfuller/data/chapter3")
# setwd("E:/chapter3/for GAMs")
g = st_read("ch3_forGAMs_poly_prefire180_bark1.gpkg")
targetcrs = st_crs(g)
g = g |> 
  dplyr::select(ID,
                stringybark)

setwd("/glade/scratch/kjfuller/data")
# setwd("D:/chapter1/bark-type-SDM/outputs/RFs/final models/final geotiffs")
r = raster("NSW_stringybark_distribution.tif")

g = st_transform(g, crs = st_crs(r))
l = raster::extract(r, g, method = 'simple')
g$stringybark.1 = unlist(lapply(l, FUN = function(x) quantile(x, probs = 0.1, na.rm = T)))
g$stringybark.9 = unlist(lapply(l, FUN = function(x) quantile(x, probs = 0.9, na.rm = T)))

setwd("/glade/scratch/kjfuller/data/chapter3")
g = st_transform(g, crs = targetcrs)
st_write(g, paste0("ch3_forGAMs_poly_prefire180_bark1.2.gpkg"), delete_dsn = T)