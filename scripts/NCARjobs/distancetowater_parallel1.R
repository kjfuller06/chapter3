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

# setwd("E:/chapter3/roadways/")
water = st_read("water_polygons_restricted10km.gpkg")
water$startdate = as.Date(water$startdate)

dates = sort(unique(water$startdate))
# first date ####
print(paste0("Iteration ", 1 , ": write raster for date ", dates[1]))

# select all features that were added during the selected times- only calculate distances for any new features, to save computation time
water_temp = water |> 
  filter(startdate == dates[1])

# create a raster of regular cells across the surface of any fires that occurred after the features were created and sampling points 100m apart, sample the distance from each sample point to each feature
iso_r = raster(ext = extent(iso), res = 100, crs = crs(iso))
iso_rast = rast(iso_r)
iso_sp = as(iso_r,"SpatialPoints")

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

water_temp$index = splitfun(water_temp)

distancefun = function(x){
  water_temp = water_temp |> 
    filter(index == x)
  
  # union all polygons to simplify distance calculation
  a_filter = st_union(water_temp)
  # convert to sp
  a_filter = as(a_filter, "Spatial")
  
  ## calculate distance between water features and raster cells
  waterMin = 
    gDistance(a_filter, iso_sp, byid=TRUE)
  ## output is a matrix with ncol == nrow(water_temp) and nrow == ncell(r_temp)
  ## columns correspond to water features, rows correspond to systematically chosen points in the selected polygons
  ## since I union'ed all water features, the distance *is* the minimum distance, no need to calculate a min
  
  values(iso_r) = as.numeric(waterMin)
  writeRaster(iso_r, paste0("distancetowater_", dates[1], "_tile", x, ".tif"), overwrite = T)
  ## no fire file written out because it includes the whole area and all fires
}

sfInit(parallel = TRUE, cpus = 24)
sfExport("distancefun", "water_temp", "iso_sp", "iso_r", "dates")
sfLibrary(raster)
sfLibrary(sf)
sfLibrary(tidyverse)
sfLibrary(geojsonsf)
sfLibrary(rjson)
sfLibrary(rgeos)
sfLibrary(terra)

sfLapply(c(1:24), distancefun)

sfStop()