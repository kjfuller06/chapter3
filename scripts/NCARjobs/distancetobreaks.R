args = commandArgs()
code = substr(args[grepl("code", args)], 6, 10)
num = as.numeric(substr(args[grepl("num", args)], 5, 10))

library(raster)
library(sf)
library(tidyverse)
library(geojsonsf)
library(rjson)
library(rgeos)

# distance to breaks ####
setwd("/glade/scratch/kjfuller/data")
# setwd("E:/chapter3/isochrons")
g = st_read("isochrons_8days.gpkg")
targetcrs = st_crs(g)
g$poly_sD = as.POSIXct(g$poly_sD)
g = g |> 
  dplyr::select(ID, poly_sD)
## length(unique(g$ID)) = 106,729

if(num == 100001){
  end = length(unique(g$ID))
} else {
  end = num + 9999
}
print(paste0("processing ", num, " through ", end))

IDs = unique(g$ID)
IDs = IDs[c(num:end)]
g = g[g$ID %in% IDs,]

num = 1
end = nrow(g)

# convert features back to simple geometries, remove Z axis and convert to projected CRS
# setwd("E:/chapter3/waterways/")
water = st_read("water_polygons.gpkg")
# setwd("E:/chapter3/roadways/")
access = st_read("access_linestrings.gpkg")

# for each polygon, create a raster of regular cells across the surface, filter features by start date and sample the distance from each raster cell to each feature
breakfun = function(x){
  g_temp = g[g$ID == IDs[x],]
  r_temp = raster(ext = extent(g_temp), res = 10, crs = crs(g_temp))
  
  water = water |> 
    filter(startdate < min(g_temp$poly_sD))
  water_temp = water[g_temp,] ## select only the water features inside the polygon
  
  ## if no water features are captured in the above subset, add a buffer around the polygon and try again
  distanceW = 1000
  while(nrow(water_temp) == 0){
    distance_checkW = distanceW
    g_buffer = st_buffer(g_temp, dist = distanceW)
    water_temp = water[g_buffer,]
    distanceW = distanceW*2
  }
  
  access = access |> 
    filter(startdate < min(g_temp$poly_sD))
  access_temp = access[g_temp,] ## select only the access lines inside the polygon
  
  ## if no access lines are captured in the above subset, add a buffer around the polygon and try again
  distanceA = 100
  while(nrow(access_temp) == 0){
    distance_checkA = distanceA
    g_buffer = st_buffer(g_temp, dist = distanceA)
    access_temp = access[g_buffer,]
    distanceA = distanceA*2
  }
  
  df = data.frame()
  ## only run distance calculation if water features are at least as close as access lines or closer
  if(distance_checkW == distance_checkA | distance_checkW < distance_checkA){
    water_temp = as(water_temp, "Spatial")
    waterMin = 
      gDistance(water_temp, as(r_temp,"SpatialPoints"), byid=TRUE)
    ## output is a matrix with ncol == nrow(water_temp) and nrow == ncell(r_temp)
    ## columns correspond to polygons or lines, rows correspond to systematically chosen points in the selected polygon
    ## calculate distance between water and raster cells
    df = waterMin
  }
  
  ## only run distance calculation if access lines are at least as close as water features or closer
  if(distance_checkW == distance_checkA | distance_checkA < distance_checkW){
    access_temp = as(access_temp, "Spatial")
    accessMin = 
      gDistance(access_temp, as(r_temp,"SpatialPoints"), byid=TRUE)
    if(nrow(df) > 0){
      df = cbind(df, accessMin)
    } else {
      df = accessMin
    }
  }

  g_temp$breaksMin = 
    median(as.numeric(
      apply(df, 1, min)))
  ## combine distances by cell, calculate the minimum for each row (each cell), calculate the median min distance to a break for each polygon
  
  st_geometry(g_temp) = NULL
  g_temp = g_temp |> 
    dplyr::select(ID, breaksMin)
  return(g_temp)
}

l = list()
for(i in c(num:end)){
  print(i)
  l[[i]] = breakfun(i)
}
l = bind_rows(l)
write.csv(l, paste0("isochron_breaks_", code, ".csv"), row.names = F)