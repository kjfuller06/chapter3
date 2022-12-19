library(raster)
library(sf)
library(tidyverse)
library(geojsonsf)
library(rjson)
library(rgeos)

# aggregate function
aggfun <- function(r_temp,ppside){
  h        <- ceiling(ncol(r_temp)/ppside)
  v        <- ceiling(nrow(r_temp)/ppside)
  agg      <- aggregate(r_temp,fact=c(h,v))
  agg[] = 1:ncell(agg)
  agg_poly = rasterToPolygons(agg)
  agg_poly = st_as_sf(agg_poly)
  
  l = list()
  # process and save tiles
  for(i in c(1:(ppside^2))){
    # read polygon crops
    e1 <- extent(agg_poly[i,])
    # crop raster to tile
    l[[i]] <- crop(r_temp, e1)
  }
  return(l)
}



# distance to breaks ####
setwd("/glade/scratch/kjfuller/data")
# setwd("E:/chapter3/isochrons")
g = st_read("isochrons_8days.gpkg")
targetcrs = st_crs(g)
g$poly_sD = as.POSIXct(g$poly_sD)
g = g |> 
  dplyr::select(ID, poly_sD)
## length(unique(g$ID)) = 106,729

# convert features back to simple geometries, remove Z axis and convert to projected CRS
# setwd("E:/chapter3/waterways/")
water = st_read("water_polygons.gpkg")
water$startdate = as.Date(water$startdate)

dates = sort(unique(water$startdate))
# for(i in c(1:length(dates))){
  print(paste0("Iteration ", i , ": write raster for dates ", dates[i], " through ", dates[i+1]))
  g_temp = g |> 
    filter(poly_sD > dates[i] & poly_sD < dates[i+1])
  
  if(nrow(g_temp) > 0){
    water_temp = water |> 
      filter(startdate <= dates[i]) |> 
      as("Spatial")
    
    # for each set of polygons, create a raster of regular cells across the surface and sample the distance from each raster cell to each feature
    r_temp = raster(ext = extent(g_temp), res = 100, crs = crs(g_temp))
    l = aggfun(r_temp=r_temp, ppside = 10)
    
    for(i in c(length(l))){
      r_r = l[[i]]
      r_sp = as(r_r,"SpatialPoints")
      
      waterMin = 
        gDistance(water_temp, r_sp, byid=TRUE)
      ## output is a matrix with ncol == nrow(water_temp) and nrow == ncell(r_temp)
      ## columns correspond to polygons or lines, rows correspond to systematically chosen points in the selected polygon
      ## calculate distance between water and raster cells
      
      values(r_r) = as.numeric(
        apply(waterMin, 1, min))
      writeRaster(r_r, paste0("distancetowater_", dates[i], "_tile", i, ".tif"), overwrite = T)
    }
    st_write(g_temp, paste0("distancetowater_polygons_", dates[i], ".gpkg"), delete_dsn = T)
  }
# }