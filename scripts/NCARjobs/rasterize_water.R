library(raster)
library(sf)
library(tidyverse)
library(geojsonsf)
library(rjson)
library(rgeos)
library(terra)
library(FNN)
library(parallel)

# load isochrons ####
setwd("/glade/scratch/kjfuller/data")
# setwd("E:/chapter3/isochrons")
iso = st_read("isochrons_8days.gpkg")
targetcrs = crs(iso)
iso$poly_sD = as.POSIXct(iso$poly_sD)
iso = iso |>
  dplyr::select(ID, poly_sD)
## length(unique(iso$ID)) = 106,729

# create raster template for rasterizing layers
iso_r = raster(ext = extent(iso), res = 100, crs = targetcrs)
rm(iso)

# waterways ####
setwd("/glade/scratch/kjfuller/data")
# setwd("E:/chapter3/waterways")
water = st_read("water.gpkg")

# convert water to Spatial format and rasterize in parallel
water_sp = as(water,"Spatial")
no_cores <- 12
# Number of polygons features in SPDF
features <- 1:length(attributes(water_sp)$polygons)
# Split features in n parts
n <- 50
parts <- split(features, cut(features, n))
# Initiate cluster (after loading all the necessary object to R environment: BRA_adm2, parts, r.raster, n)
cl <- makeCluster(no_cores, type = "FORK")
print(cl)
# Parallelize rasterize function
water_pieces <- parLapply(cl = cl, X = 1:n, fun = function(x) rasterize(water_sp[parts[[x]],], iso_r, field = 1))
stopCluster(cl)
# Merge all raster parts
water_r <- do.call(merge, water_pieces)
writeRaster(water_r, "water_parallel.tif", overwrite = T)

# convert template raster and water raster to data frames and grab just the coordinates of the locations of interest (for water, the non-NA values; for the query raster, all cells)
water_df = as.data.frame(water_r, xy = TRUE)
water_df = water_df[!is.na(water_df[,3]),1:2]

iso_df = as.data.frame(iso_r, xy = TRUE)
iso_df = iso_df[,1:2]

# calculate the distance between each set of points- returns 1 value per query cells (iso_df)
dnear = knnx.dist(data = water_df, query = iso_df, k = 1)

# assign the values to the NA cells in the first raster
water_r = iso_r
values(water_r) = dnear
writeRaster(water_r, "distancetowater_parallel.tif", overwrite = T)

