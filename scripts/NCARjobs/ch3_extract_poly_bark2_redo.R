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

setwd("/glade/scratch/kjfuller/data/RFs")
# setwd("D:/chapter1/bark-type-SDM/outputs/RFs/final models/final geotiffs")
# r = raster("NSW_ribboning_distribution.tif")
r = raster("predmosaic_RF12_high_09_allfires.tif")
threshold = 1 - 0.24 ## values are inverted for ribbonbarks
# values(rpred)[values(rpred) > threshold] = 0
# values(rpred)[values(rpred) <= threshold] = 1
rcl = matrix(c(-Inf, threshold,
               threshold, Inf,
               0, 1), 
             ncol = 3, byrow = F)
r = reclassify(x = r, rcl = rcl, right = F, include.lowest = T)
setwd("/glade/scratch/kjfuller/data")
writeRaster(r, "RF12_ribboning_classified.tif", overwrite = T)
# r = raster("RF12_ribboning_classified.tif")

g = st_transform(g, crs = st_crs(r))
g$ribbonbark = raster::extract(r, g, method = 'simple', fun = median, na.rm = T)

setwd("/glade/scratch/kjfuller/data/chapter3")
g = st_transform(g, crs = targetcrs)
st_write(g, paste0("ch3_forGAMs_poly_prefire180_bark2_redo.gpkg"), delete_dsn = T)

# load isochrons ####
setwd("/glade/scratch/kjfuller/data/chapter3")
# setwd("E:/chapter3/for GAMs")
g = st_read("ch3_forGAMs_poly_prefire180_bark2_redo.gpkg")
targetcrs = st_crs(g)
g = g |> 
  dplyr::select(ID,
                ribbonbark)

setwd("/glade/scratch/kjfuller/data")
# setwd("D:/chapter1/bark-type-SDM/outputs/RFs/final models/final geotiffs")
r = raster("RF12_ribboning_classified.tif")

g = st_transform(g, crs = st_crs(r))
l = raster::extract(r, g, method = 'simple')
g$ribbonbark.1 = unlist(lapply(l, FUN = function(x) quantile(x, probs = 0.1, na.rm = T)))
g$ribbonbark.9 = unlist(lapply(l, FUN = function(x) quantile(x, probs = 0.9, na.rm = T)))

setwd("/glade/scratch/kjfuller/data/chapter3")
g = st_transform(g, crs = targetcrs)
st_write(g, paste0("ch3_forGAMs_poly_prefire180_bark2.2_redo.gpkg"), delete_dsn = T)