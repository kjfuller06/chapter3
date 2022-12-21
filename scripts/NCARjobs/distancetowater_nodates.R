library(raster)
library(sf)
library(tidyverse)
library(geojsonsf)
library(rjson)
library(rgeos)
library(terra)
library(doParallel)
library(snowfall)

# load files ####
setwd("/glade/scratch/kjfuller/data")
# setwd("E:/chapter3/isochrons")
iso = st_read("isochrons_8days.gpkg")
targetcrs = st_crs(iso)
iso$poly_sD = as.POSIXct(iso$poly_sD)
iso = iso |> 
  dplyr::select(ID, poly_sD)
## length(unique(iso$ID)) = 106,729

# # setwd("E:/chapter3/roadways/")
# water = st_read("water_polygons.gpkg")
# water$startdate = as.Date(water$startdate)
# 
# # to start, remove all features which are not within 10km of any fire
# iso_ext = st_as_sf(as.polygons(ext(iso)))
# st_crs(iso_ext) = targetcrs
# iso_ext = st_buffer(iso_ext, dist = 10000)
# water = water[iso_ext,]
# st_write(water, "water_polygons_restricted10km.gpkg", delete_dsn = T)

water = st_read("water_polygons_restricted10km.gpkg")

# for each set of features, create a raster of regular cells across the surface of any fires that occurred after the features was created and sample the distance from each raster cell to each feature
iso_r = raster(ext = extent(iso), res = 100, crs = crs(iso))

# convert raster to sampling points 100m apart
iso_sp = as(iso_r,"SpatialPoints")

# limit features to only those features which fall within 10km of the extent of the sampling area, to save computation time
iso_ext = st_as_sf(as.polygons(ext(iso_r)))
st_crs(iso_ext) = targetcrs
iso_ext = st_buffer(iso_ext, dist = 10000)
w_filter = water[iso_ext,]

splitfun = function(x){
  r = nrow(x)
  n = r/12
  rn = floor(n)
  ID = c(rep(1, rn),
         rep(2, rn),
         rep(3, rn),
         rep(4, rn),
         rep(5, rn),
         rep(6, rn),
         rep(7, rn),
         rep(8, rn),
         rep(9, rn),
         rep(10, rn),
         rep(11, rn),
         rep(12, rn))
  if((r - length(ID)) > 0){
    ID = c(ID, rep(12, (r - length(ID))))
  }
  return(ID)
}

w_filter$index = splitfun(w_filter)

distancefun = function(x){
w_filter = w_filter |>
  filter(index == x)
  
  # union all polygons to simplify distance calculation
  w_filter = st_union(w_filter)
  # convert to sp
  w_filter = as(w_filter, "Spatial")
  
  ## calculate distance between water features and raster cells
  waterMin = 
    gDistance(w_filter, iso_sp, byid=TRUE)
  ## output is a matrix with ncol == nrow(water_temp) and nrow == ncell(r_temp)
  ## columns correspond to water lines, rows correspond to systematically chosen points in the selected polygons
  ## since I union'ed all water features, the distance *is* the minimum distance, no need to calculate a min
  
  values(iso_r) = as.numeric(waterMin)
  # writeRaster(iso_r, paste0("distancetowater_layer", x, ".tif"), overwrite = T)
  writeRaster(iso_r, paste0("distancetowater_layer", x, ".tif"), overwrite = T)
}

sfInit(parallel = TRUE, cpus = 12)
sfExport("distancefun", "w_filter", "iso_sp", "iso_r")
sfLibrary(raster)
sfLibrary(sf)
sfLibrary(tidyverse)
sfLibrary(geojsonsf)
sfLibrary(rjson)
sfLibrary(rgeos)
sfLibrary(terra)

sfLapply(c(1:12), distancefun)

sfStop()

