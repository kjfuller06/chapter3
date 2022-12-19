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
# setwd("E:/chapter3/roadways/")
access = st_read("access_linestrings.gpkg")
access$startdate = as.Date(access$startdate)

dates = sort(unique(access$startdate))
for(i in c(1:length(dates))){
  print(paste0("Iteration ", i , ": write raster for date ", dates[i]))
  # select isocrons by date
  iso_temp = iso |> 
    filter(poly_sD >= dates[i] & poly_sD <= dates[i+1])
  
  if(nrow(iso_temp) > 0){
    # select access linestrings by date (will need to take the min of all relevant rasters, running through each date of "access" and grabbing all previous rasters, as each date represents new, additional access features to include in potential minimum distance)
    access_temp = access |> 
      filter(startdate == dates[i])
    
    # for each set of polygons, create a raster of regular cells across the surface and sample the distance from each raster cell to each feature
    iso_r = raster(ext = extent(iso_temp), res = 100, crs = crs(iso_temp))
    iso_rast = rast(iso_r)
    
    # convert raster to sampling points 100m apart
    iso_sp = as(iso_r,"SpatialPoints")
    
    # select intersecting access linestrings, sampling a larger buffered region until overlapping features are found
    iso_ext = st_as_sf(as.polygons(ext(iso_r)))
    st_crs(iso_ext) = targetcrs
    a_filter = access_temp[iso_ext,]
    
    # expand buffer if necessary
    distance = 1000
    while(nrow(a_filter) == 0){
      iso_ext = st_buffer(iso_ext, dist = distance)
      a_filter = access_temp[iso_ext,]
    }
    
    # union all polygons to simplify distance calculation
    a_filter = st_union(a_filter)
    # convert selected access features to SP
    a_filter = as(a_filter, "Spatial")
    
    ## calculate distance between access features and raster cells
    accessMin = 
      gDistance(a_filter, iso_sp, byid=TRUE)
    ## output is a matrix with ncol == nrow(access_temp) and nrow == ncell(r_temp)
    ## columns correspond to access lines, rows correspond to systematically chosen points in the selected polygons
    ## since I union'ed all access features, the distance *is* the minimum distance, no need to calculate a min
    
    values(iso_r) = as.numeric(accessMin)
    writeRaster(iso_r, paste0("distancetoaccess_", dates[i], ".tif"), overwrite = T)
    st_write(iso_temp, paste0("distancetoaccess_polygons_after", dates[i], ".gpkg"), delete_dsn = T)
  }
}