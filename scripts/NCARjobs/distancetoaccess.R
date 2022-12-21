args = commandArgs()
num = as.numeric(substr(args[grepl("num", args)], 5, 10))

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
# setwd("/glade/scratch/kjfuller/data")
setwd("E:/chapter3/isochrons")
iso = st_read("isochrons_8days.gpkg")
targetcrs = st_crs(iso)
iso$poly_sD = as.POSIXct(iso$poly_sD)
iso = iso |> 
  dplyr::select(ID, poly_sD)
## length(unique(iso$ID)) = 106,729

setwd("E:/chapter3/roadways/")
access = st_read("access_lines.gpkg")
access = st_transform(access, crs = targetcrs)
access$startdate = as.Date(access$startdate)

# to start, remove all features which are not within 10km of any fire
iso_ext = st_as_sf(as.polygons(ext(iso)))
st_crs(iso_ext) = targetcrs
iso_ext = st_buffer(iso_ext, dist = 10000)
access = access[iso_ext,]
access = st_zm(access, drop = TRUE, what = "ZM")
st_write(access, "access_lines_restricted10km.gpkg", delete_dsn = T)

access = st_read("access_lines_restricted10km.gpkg")
access$startdate = as.Date(access$startdate)

dates = sort(unique(access$startdate))

# setwd("E:/chapter3/roadways")
skip = st_read("access_skipdates.gpkg")
st_geometry(skip) = NULL
dates = dates[!dates %in% skip$date]
print(paste0("length dates = ", length(dates)))

# all other intervals ####
print(paste0("Iteration ", num , ": write raster tiles for dates ", dates[num], " to ", dates[num + 1]))

# select all fires that occurred *after* all selected features were added (all fires for which the features were relevant)
# select access features by date (will need to take the min of all relevant rasters, running through each date of "access" and grabbing all previous rasters, as each date represents new, additional access features to include in potential minimum distance)
iso_temp = iso |> 
  filter(poly_sD >= dates[num + 1])

# select all features that were added during the selected times- only calculate distances for any new features, to save computation time
access_temp = access |> 
  filter(startdate > dates[num] & startdate <= dates[num + 1])

# for each set of features, create a raster of regular cells across the surface of any fires that occurred after the features was created and sample the distance from each raster cell to each feature
iso_r = raster(ext = extent(iso_temp), res = 100, crs = crs(iso_temp))

if(ncell(iso_r) > 1){
  # convert raster to sampling points 100m apart
  iso_sp = as(iso_r,"SpatialPoints")
} else {
  iso_sp = as(as(st_centroid(iso_temp), "Spatial"), "SpatialPoints")
}

# limit features to only those features which fall within 10km of the extent of the sampling area, to save computation time
iso_ext = st_as_sf(as.polygons(ext(iso_r)))
st_crs(iso_ext) = targetcrs
iso_ext = st_buffer(iso_ext, dist = 10000)
w_filter = access_temp[iso_ext,]

# tm_shape(iso_ext) + tm_borders() + tm_shape(access_temp) + tm_lines()

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

if(nrow(w_filter) > 0){
  w_filter$index = splitfun(w_filter)
  
  distancefun = function(x){
    w_filter = w_filter |> 
      filter(index == x)
    
    # union all polygons to simplify distance calculation
    w_filter = st_union(w_filter)
    # convert to sp
    w_filter = as(w_filter, "Spatial")
    
    ## calculate distance between access features and raster cells
    accessMin = 
      gDistance(w_filter, iso_sp, byid=TRUE)
    ## output is a matrix with ncol == nrow(access_temp) and nrow == ncell(r_temp)
    ## columns correspond to access lines, rows correspond to systematically chosen points in the selected polygons
    ## since I union'ed all access features, the distance *is* the minimum distance, no need to calculate a min
    
    values(iso_r) = as.numeric(accessMin)
    writeRaster(iso_r, paste0("distancetoaccess_", dates[num], "_tile", x, ".tif"), overwrite = T)
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
  
  st_write(iso_temp, paste0("distancetoaccess_linestrings_after_", dates[x + 1], ".gpkg"), delete_dsn = T)
} else {
  print(paste0("no relevant features added for dates ", dates[num], " to ", dates[num + 1]))
}
