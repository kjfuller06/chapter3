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
# first date ####
print(paste0("Iteration ", 1 , ": write raster for date ", dates[1]))

# select all features that were added during the selected times- only calculate distances for any new features, to save computation time
water_temp = water |> 
  filter(startdate == dates[1])

# create a raster of regular cells across the surface of any fires that occurred after the features were created and sampling points 100m apart, sample the distance from each sample point to each feature
iso_r = raster(ext = extent(iso), res = 100, crs = crs(iso))
iso_rast = rast(iso_r)
iso_sp = as(iso_r,"SpatialPoints")

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
writeRaster(iso_r, paste0("distancetowater_", dates[1], ".tif"), overwrite = T)
## no fire file written out because it includes the whole area and all fires

# now run for all other intervals
for(i in c(1:length(dates))){
  print(paste0("Iteration ", i , ": write raster for date ", dates[i]))
  # check if any fires occurred between the dates when features were added
  iso_temp = iso |> 
    filter(poly_sD > dates[1] & poly_sD <= dates[2])
  
  if(nrow(iso_temp) == 0){
    dates = dates[-(i+1)]
  } else {
    # select all fires that occurred *after* all selected features were added (all fires for which the features were relevant)
    # select water features by date (will need to take the min of all relevant rasters, running through each date of "water" and grabbing all previous rasters, as each date represents new, additional water features to include in potential minimum distance)
    iso_temp = iso |> 
      filter(poly_sD >= dates[2])
    
    # select all features that were added during the selected times- only calculate distances for any new features, to save computation time
    water_temp = water |> 
      filter(startdate > dates[1] & startdate <= dates[2])
    
    # for each set of features, create a raster of regular cells across the surface of any fires that occurred after the features was created and sample the distance from each raster cell to each feature
    iso_r = raster(ext = extent(iso_temp), res = 100, crs = crs(iso_temp))
    iso_rast = rast(iso_r)
    
    # convert raster to sampling points 100m apart
    iso_sp = as(iso_r,"SpatialPoints")
    
    # limit features to only those features which fall within 10km of the extent of the sampling area, to save computation time
    iso_ext = st_as_sf(as.polygons(ext(iso_r)))
    st_crs(iso_ext) = targetcrs
    iso_ext = st_buffer(iso_ext, dist = 10000)
    a_filter = water_temp[iso_ext,]
    
    # if no features were added within 10km of a fire, progress to the next time without writing raster or fire file; no need to calculate distance to very distant features and no need to write out fire polygons, since these fires have distance calculations from the previous timeframe (writing them out would just duplicate the entries for fires with no additional calculated distances)
    if(nrow(a_filter) > 0){
      # union all polygons to simplify distance calculation
      a_filter = st_union(a_filter)
      # convert selected water features to sp
      a_filter = as(a_filter, "Spatial")
      
      ## calculate distance between water features and raster cells
      waterMin = 
        gDistance(a_filter, iso_sp, byid=TRUE)
      ## output is a matrix with ncol == nrow(water_temp) and nrow == ncell(r_temp)
      ## columns correspond to water features, rows correspond to systematically chosen points in the selected polygons
      ## since I union'ed all water features, the distance *is* the minimum distance, no need to calculate a min
      
      values(iso_r) = as.numeric(waterMin)
      writeRaster(iso_r, paste0("distancetowater_", dates[2], ".tif"), overwrite = T)
      st_write(iso_temp, paste0("distancetowater_polygons_after_", dates[2], ".gpkg"), delete_dsn = T)
    }
    # regardless of whether features were relevant, move to the next date, as these features have been ruled out
    dates = dates[-i]
  }
}