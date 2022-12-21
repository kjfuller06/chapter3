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

water = st_read("water_polygons_restricted10km.gpkg")
water$startdate = as.Date(water$startdate)

dates = sort(unique(water$startdate))
dates_perm = dates
df = data.frame()
x = 1
# now run for all other intervals
for(i in c(1:length(dates_perm))){
  print(paste0("Iteration ", i , ": write raster for dates ", dates[1], " to ", dates[2]))
  # check if any fires occurred between the dates when features were added
  iso_temp = iso |> 
    filter(poly_sD > dates[1] & poly_sD <= dates[2])
  
  if(nrow(iso_temp) == 0){
    df_temp = data.frame(index = x, date = dates[2])
    df = rbind(df, df_temp)
    dates = dates[-2]
    x = x + 1
  } else {
    # select all fires that occurred *after* all selected features were added (all fires for which the features were relevant)
    # select water features by date (will need to take the min of all relevant rasters, running through each date of "water" and grabbing all previous rasters, as each date represents new, additional water features to include in potential minimum distance)
    iso_temp = iso |> 
      filter(poly_sD >= dates[2])
    
    # select all features that were added during the selected times- only calculate distances for any new features, to save computation time
    water_temp = water |> 
      filter(startdate > dates[1] & startdate <= dates[2])
    
    iso_r = raster(ext = extent(iso_temp), res = 100, crs = crs(iso_temp))
    
    # limit features to only those features which fall within 10km of the extent of the sampling area, to save computation time
    iso_ext = st_as_sf(as.polygons(ext(iso_r)))
    st_crs(iso_ext) = targetcrs
    iso_ext = st_buffer(iso_ext, dist = 10000)
    a_filter = water_temp[iso_ext,]
    
    if(nrow(a_filter) > 0){
      df_temp = data.frame(index = x, date = dates[1])
      df = rbind(df, df_temp)
      dates = dates[-1]
      x = x + 1
    }
  }
}
df$lat = 0
df$lon = 0
df = st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
st_write(df, "skipdates.gpkg", delete_dsn = T)