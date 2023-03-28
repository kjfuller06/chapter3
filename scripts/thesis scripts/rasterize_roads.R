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

# roads ####
setwd("/glade/scratch/kjfuller/data")
# setwd("E:/chapter3/roadways")
roads = st_read("roads_restricted10km.gpkg")

# convert roads to SpatialLines format and rasterize in parallel
road_sp = as(as(roads,"Spatial"), "SpatialLines")
no_cores <- 12
# Number of polygons features in SPDF
features <- 1:length(attributes(road_sp)$lines)
# Split features in n parts
n <- 50
parts <- split(features, cut(features, n))
# Initiate cluster (after loading all the necessary object to R environment: BRA_adm2, parts, r.raster, n)
cl <- makeCluster(no_cores, type = "FORK")
print(cl)
# Parallelize rasterize function
road_pieces <- parLapply(cl = cl, X = 1:n, fun = function(x) rasterize(road_sp[parts[[x]],], iso_r, field = 1))
stopCluster(cl)
# Merge all raster parts
road_r <- do.call(merge, road_pieces)
writeRaster(road_r, "roads.tif", overwrite = T)

# convert template raster and roads raster to data frames and grab just the coordinates of the locations of interest (for roads, the non-NA values; for the query raster, all cells)
road_df = as.data.frame(road_r, xy = TRUE)
road_df = road_df[!is.na(road_df[,3]),1:2]

iso_df = as.data.frame(iso_r, xy = TRUE)
iso_df = iso_df[,1:2]

# calculate the distance between each set of points- returns 1 value per query cells (iso_df)
dnear = knnx.dist(data = road_df, query = iso_df, k = 1)

# assign the values to the NA cells in the first raster
road_r = iso_r
values(road_r) = dnear
writeRaster(road_r, "distancetoroads.tif", overwrite = T)