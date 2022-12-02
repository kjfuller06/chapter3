library(sf)
library(raster)
library(tidyverse)
library(exactextractr)

# setwd("/glade/scratch/kjfuller/data")
extractfun = function(x){
  g = paste0("Ch3_allfiredata_prefire", x, ".gpkg")
  targetcrs = st_crs(g)
  
  # load rasters
  # slope
  setwd("D:/chapter1/other_data/Final/terrain variables")
  r = raster("proj_dem_slope_30m.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g_buffer = st_buffer(g, dist = 12.5)
  
  g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
  names(g_temp)[names(g_temp) == "mode"] = "slope"
  g = left_join(g, g_temp)
  
  # northness
  r = raster("proj_dem_northness_30m.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g_buffer = st_buffer(g, dist = 12.5)
  
  g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
  names(g_temp)[names(g_temp) == "mode"] = "northness"
  g = left_join(g, g_temp)
  
  # eastness
  r = raster("proj_dem_eastness_30m.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g_buffer = st_buffer(g, dist = 12.5)
  
  g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
  names(g_temp)[names(g_temp) == "mode"] = "eastness"
  g = left_join(g, g_temp)
  
  # bark types
  r = raster("NSW_stringybark_distribution.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g_buffer = st_buffer(g, dist = 12.5)
  
  g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
  names(g_temp)[names(g_temp) == "mode"] = "stringybark"
  g = left_join(g, g_temp)
  
  r = raster("NSW_ribboning_distribution.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g_buffer = st_buffer(g, dist = 12.5)
  
  g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
  names(g_temp)[names(g_temp) == "mode"] = "ribbonbark"
  g = left_join(g, g_temp)
  
  g = st_transform(g, crs = targetcrs)
  st_write(g, "Ch3_forGAMs_prefire", x, ".gpkg", delete_dsn = T)
}

extractfun(7)
extractfun(14)
extractfun(30)
extractfun(60)
extractfun(90)
extractfun(180)
