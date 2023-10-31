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
setwd("E:/chapter3/isochrons")
iso = st_read("isochrons_8days.gpkg")
targetcrs = crs(iso)
iso$poly_sD = as.POSIXct(iso$poly_sD)
iso = iso |>
  dplyr::select(ID, poly_sD)
## length(unique(iso$ID)) = 106,729

# create raster template for rasterizing layers
iso_r = raster(ext = extent(iso), res = 100, crs = targetcrs)
rm(iso)
iso_df = as.data.frame(iso_r, xy = TRUE)
iso_df = iso_df[,1:2]

# waterways ####
setwd("D:/chapter3/climatedem")
# water = rast("streamorder.map")
# water = terra::project(water, y = targetcrs, method = "near")
# writeRaster(water, "streamorder_proj.tif", overwrite = T)

water = rast("streamorder_proj.tif")

memory.limit(10000000) ## not enough memory still
setwd("E:/chapter3/waterways")
water_poly = st_read("hydro_nonpere_restricted10km.gpkg")

raster::extract(x = water, y = water_poly[1,]) ## extracts multiple values of Strahler orders. This also grabs order classifications from feeder streams that might not actually exist- the whole reason I wanted to extract these values in the first place.

for(i in c(12:1)){
  water1 = raster(clamp(water, lower = i, upper = i))

  # convert template raster and water raster to data frames and grab just the coordinates of the locations of interest (for water, the non-NA values; for the query raster, all cells)
  water_df = as.data.frame(water1, xy = TRUE, na.rm = T)
  water_df = water_df[!is.na(water_df[,3]),1:2]

  # calculate the distance between each set of points- returns 1 value per query cells (iso_df)
  dnear = knnx.dist(data = water_df, query = iso_df, k = 1)

  # assign the values to the NA cells in the first raster
  water_r = iso_r
  values(water_r) = dnear
  writeRaster(water_r, paste0("distancetoStrahler_", i, ".tif"), overwrite = T)
}
