num = 1

library(raster)
library(sf)
library(tidyverse)
library(geojsonsf)
library(rjson)
library(rgeos)
library(terra)

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
water$startdate = as.Date(water$startdate)

dates = sort(unique(water$startdate))

# setwd("E:/chapter3/roadways")
skip = st_read("water_skipdates.gpkg")
st_geometry(skip) = NULL
dates = dates[!dates %in% skip$date]
print(paste0("length dates = ", length(dates)))

# all other intervals ####
  print(paste0("Iteration ", num , ": write raster tiles for all dates after ", dates[num]))
  
  # select all fires that occurred *after* all selected features were added (all fires for which the features were relevant)
  # select water features by date (will need to take the min of all relevant rasters, running through each date of "water" and grabbing all previous rasters, as each date represents new, additional water features to include in potential minimum distance)
  iso_temp = iso |> 
    filter(poly_sD >= dates[num])
  
  # select all features that were added during the selected times- only calculate distances for any new features, to save computation time
  water_temp = water |> 
    filter(startdate > dates[num] & startdate <= dates[num + 1])
  
  # for each set of features, create a raster of regular cells across the surface of any fires that occurred after the features was created and sample the distance from each raster cell to each feature
  iso_r = raster(ext = extent(iso_temp), res = 100, crs = crs(iso_temp))
  
  # convert raster to sampling points 100m apart
  iso_sp = as(iso_r,"SpatialPoints")
  
  # limit features to only those features which fall within 10km of the extent of the sampling area, to save computation time
  iso_ext = st_as_sf(as.polygons(ext(iso_r)))
  st_crs(iso_ext) = targetcrs
  iso_ext = st_buffer(iso_ext, dist = 10000)
  w_filter = water_temp[iso_ext,]

splitfun = function(x){
  r = nrow(x)
  n = r/24
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
         rep(12, rn),
         rep(13, rn),
         rep(14, rn),
         rep(15, rn),
         rep(16, rn), 
         rep(17, rn), 
         rep(18, rn), 
         rep(19, rn), 
         rep(20, rn), 
         rep(21, rn), 
         rep(22, rn), 
         rep(23, rn), 
         rep(24, rn))
  if((r - length(ID)) > 0){
    ID = c(ID, rep(24, (r - length(ID))))
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
  writeRaster(iso_r, paste0("distancetowater_", dates[num], "_tile", x, ".tif"), overwrite = T)
}

sfInit(parallel = TRUE, cpus = 24)
sfExport("distancefun", "w_filter", "iso_sp", "iso_r", "dates", "num")
sfLibrary(raster)
sfLibrary(sf)
sfLibrary(tidyverse)
sfLibrary(geojsonsf)
sfLibrary(rjson)
sfLibrary(rgeos)
sfLibrary(terra)

sfLapply(c(1:24), distancefun)

sfStop()
