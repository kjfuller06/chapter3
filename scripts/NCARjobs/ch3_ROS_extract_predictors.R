library(sf)
library(raster)
library(tidyverse)

setwd("E:/chapter3/isochrons/Progall_ffdi_v3")
# setwd("/glade/scratch/kjfuller/data")
isochrons = st_read("progall_ffdi_v3.shp")
isochrons = isochrons[1:100,]
isochrons$ID = c(1:nrow(isochrons))
isochrons$time = as.POSIXct(isochrons$time)
isochrons$lasttim = as.POSIXct(isochrons$lasttim)

# load rasters
# slope
setwd("D:/chapter1/other_data/Final/terrain variables")
slope = raster("proj_dem_slope_30m.tif")

isochrons = st_transform(isochrons, crs = st_crs(slope))
slope = crop(slope, isochrons)
slope = mask(slope, isochrons)
slope = st_as_sf(rasterToPoints(slope, spatial = TRUE))
inter = st_intersection(isochrons, slope)
slope = inter
st_geometry(slope) = NULL
slope = aggregate(data = slope, proj_dem_slope_30m ~ ID, FUN = median)
isochrons = isochrons |> 
  left_join(slope)
names(isochrons)[names(isochrons) == "proj_dem_slope_30m"] = "median_slope"

# northness
setwd("D:/chapter1/other_data/Final/terrain variables")
north = raster("proj_dem_northness_30m.tif")

isochrons = st_transform(isochrons, crs = st_crs(north))
north = crop(north, isochrons)
north = mask(north, isochrons)
north = st_as_sf(rasterToPoints(north, spatial = TRUE))
inter = st_intersection(isochrons, north)
north = inter
st_geometry(north) = NULL
north = aggregate(data = north, proj_dem_northness_30m ~ ID, FUN = median)
isochrons = isochrons |> 
  left_join(north)
names(isochrons)[names(isochrons) == "proj_dem_northness_30m"] = "median_north"

# eastness
# aspect = raster("proj_dem_aspect_30m.tif")
# 
# eastness = aspect
# eastness = cos(eastness * pi / 180)
# writeRaster(eastness, "proj_dem_eastness_30m.tif")

east = raster("proj_dem_eastness_30m.tif")

isochrons = st_transform(isochrons, crs = st_crs(east))
east = crop(east, isochrons)
east = mask(east, isochrons)
east = st_as_sf(rasterToPoints(east, spatial = TRUE))
inter = st_intersection(isochrons, east)
east = inter
st_geometry(east) = NULL
east = aggregate(data = east, proj_dem_eastness_30m ~ ID, FUN = median)
isochrons = isochrons |> 
  left_join(east)
names(isochrons)[names(isochrons) == "proj_dem_eastness_30m"] = "median_east"

