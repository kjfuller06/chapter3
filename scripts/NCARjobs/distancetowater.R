library(raster)
library(sf)
library(tidyverse)
library(geojsonsf)
library(rjson)
library(rgeos)
library(terra)

# distance to breaks ####
setwd("/glade/scratch/kjfuller/data")
# setwd("E:/chapter3/isochrons")
iso = st_read("isochrons_8days.gpkg")
targetcrs = st_crs(iso)
iso$poly_sD = as.POSIXct(iso$poly_sD)
iso = iso |> 
  dplyr::select(ID, poly_sD)
## length(unique(iso$ID)) = 106,729

# convert features back to simple geometries, remove Z axis and convert to projected CRS
# setwd("E:/chapter3/waterways/")
water = st_read("water_polygons.gpkg")
water$startdate = as.Date(water$startdate)

dates = sort(unique(water$startdate))
for(i in c(1:length(dates))){
  print(paste0("Iteration ", i , ": write raster for date ", dates[i]))
  # select isocrons by date
  iso_temp = iso |> 
    filter(poly_sD >= dates[i] & poly_sD <= dates[i+1])
  
  if(nrow(iso_temp) > 0){
    # select water polygons by date (will need to take the min of all relevant rasters, running through each date of "water" and grabbing all previous rasters, as each date represents new, additional water features to include in potential minimum distance)
    water_temp = water |> 
      filter(startdate == dates[i])
    
    # for each set of polygons, create a raster of regular cells across the surface and sample the distance from each raster cell to each feature
    iso_r = raster(ext = extent(iso_temp), res = 100, crs = crs(iso_temp))
    iso_rast = rast(iso_r)
    
    # convert raster to sampling points 100m apart
    iso_sp = as(iso_r,"SpatialPoints")
    
    # select intersecting water polygons, sampling a larger buffered region until overlapping features are found
    iso_ext = st_as_sf(as.polygons(ext(iso_r)))
    st_crs(iso_ext) = targetcrs
    w_filter = water_temp[iso_ext,]
    
    # expand buffer if necessary
    distance = 1000
    while(nrow(w_filter) == 0){
      iso_ext = st_buffer(iso_ext, dist = distance)
      w_filter = water_temp[iso_ext,]
    }
    
    # union all polygons to simplify distance calculation
    w_filter = st_union(w_filter)
    # convert selected water features to SP
    w_filter = as(w_filter, "Spatial")
    
    ## calculate distance between water features and raster cells
    waterMin = 
      gDistance(w_filter, iso_sp, byid=TRUE)
    ## output is a matrix with ncol == nrow(water_temp) and nrow == ncell(r_temp)
    ## columns correspond to polygons or lines, rows correspond to systematically chosen points in the selected polygon
    ## since I union'ed all water features, the distance *is* the minimum distance, no need to calculate a min
    
    values(iso_r) = as.numeric(waterMin)
    writeRaster(iso_r, paste0("distancetowater_", dates[i], ".tif"), overwrite = T)
    st_write(iso_temp, paste0("distancetowater_polygons_after", dates[i], ".gpkg"), delete_dsn = T)
  }
}