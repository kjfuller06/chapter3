library(sf)
library(raster)
library(tidyverse)
library(exactextractr)
library(rgeos)

setwd("/glade/scratch/kjfuller/data/chapter3")
# setwd("E:/chapter3/dwellings")
houses = read.csv("housing_isochrons_density.csv") |> 
  dplyr::select(ID, house.density)

setwd("/glade/scratch/kjfuller/data")
# setwd("D:/chapter1/bark-type-SDM/data")
fire_reg = read.csv("fire_regimes.csv") |> 
  dplyr::select(fueltype,
                fire_reg)

# load forest structure data ####
setwd("/glade/scratch/kjfuller/data/chapter3")
# setwd("E:/chapter3/GEDI_FESM/")
gedi = st_read(paste0("ch3_isochrons_prefire180.gpkg"))
targetcrs = st_crs(gedi)
st_geometry(gedi) = NULL
gedi = gedi |> 
  dplyr::select(shot_number,
                ID,
                rh98,
                cover_z_1,
                over_cover,
                fhd_normal)

# load isochrons ####
setwd("/glade/scratch/kjfuller/data")
# setwd("E:/chapter3/isochrons")
g = st_read("isochrons_8days.gpkg")

# merge housing density, distance to breaks, and fire types ####
g = g |> 
  left_join(houses) 

# calculate and merge structural data ####
g_agg = g |> 
  inner_join(gedi)
# calculate the median of continuous variables
g_agg1 = aggregate(data = g_agg, rh98 ~ ID, FUN = median)
g_agg2 = aggregate(data = g_agg, cover_z_1 ~ ID, FUN = median)
g_agg3 = aggregate(data = g_agg, over_cover ~ ID, FUN = median)
g_agg4 = aggregate(data = g_agg, fhd_normal ~ ID, FUN = median)

print("joining all median values of GEDI structure to isochron data")
g = g |> 
  right_join(g_agg1) |> 
  right_join(g_agg2) |> 
  right_join(g_agg3) |> 
  right_join(g_agg4)

# load rasters
# setwd("D:/chapter1/other_data/Final/terrain variables")

# elevation ####
print("reading elevation file")
r = raster("proj_dem_s.tif")

print("converting to elevation crs")
g = st_transform(g, crs = st_crs(r))
print("extracting elevation")
elevation = raster::extract(r, g, method = 'simple')
g$elevation = sd(unlist(elevation), na.rm = T)

# fire regimes ####
# setwd("D:/chapter1/bark-type-SDM/data")
r = raster("fuels_30m.tif")

g = st_transform(g, crs = st_crs(r))
fueltype = raster::extract(r, g, method = 'simple')
fueltype = fueltype[!is.na(fueltype)]
uniqv <- unique(unlist(fueltype))
g$fueltype = uniqv[which.max(tabulate(match(unlist(fueltype), uniqv)))]
g = g |> 
  left_join(fire_reg)

# setwd("D:/chapter1/other_data/Final/terrain variables")
# slope ####
r = raster("proj_dem_slope_30m.tif")

g = st_transform(g, crs = st_crs(r))
g$slope = raster::extract(r, g, method = 'simple', fun = median, na.rm = T)

# aspect ####
r = raster("proj_dem_aspect_30m.tif")

g = st_transform(g, crs = st_crs(r))
g$aspect = raster::extract(r, g, method = 'simple', fun = median, na.rm = T)

# distance to firelines ####
r = raster("distancetofirelines_parallel.tif")

g = st_transform(g, crs = st_crs(r))
g$firelines = raster::extract(r, g, method = 'simple', fun = median, na.rm = T)

# distance to roads ####
r = raster("distancetoroads.tif")

g = st_transform(g, crs = st_crs(r))
g$roads = raster::extract(r, g, method = 'simple', fun = median, na.rm = T)

# distance to water ####
r = raster("distancetowater_relevance2.tif")

g = st_transform(g, crs = st_crs(r))
g$water2 = raster::extract(r, g, method = 'simple', fun = median, na.rm = T)

r = raster("distancetowater_relevance3.tif")
g$water3 = raster::extract(r, g, method = 'simple', fun = median, na.rm = T)

r = raster("distancetowater_relevance4.tif")
g$water4 = raster::extract(r, g, method = 'simple', fun = median, na.rm = T)

# r = raster("distancetowater_relevance5.tif")
# g$water5 = raster::extract(r, g, method = 'simple', fun = median, na.rm = T)
# 
# r = raster("distancetowater_relevance6.tif")
# g$water6 = raster::extract(r, g, method = 'simple', fun = median, na.rm = T)

# # northness
# r = raster("proj_dem_northness_30m.tif")
# 
# g = st_transform(g, crs = st_crs(r))
# g$northness = raster::extract(r, g, method = 'simple', fun = median, na.rm = T)
# 
# # eastness
# r = raster("proj_dem_eastness_30m.tif")
# g$eastness = raster::extract(r, g, method = 'simple', fun = median, na.rm = T)

# bark types ####
r = raster("NSW_stringybark_distribution.tif")

g = st_transform(g, crs = st_crs(r))
g$stringybark = raster::extract(r, g, method = 'simple', fun = median, na.rm = T)

r = raster("NSW_ribboning_distribution.tif")
g$ribbonbark = raster::extract(r, g, method = 'simple', fun = median, na.rm = T)

setwd("/glade/scratch/kjfuller/data/chapter3")
g = st_transform(g, crs = targetcrs)
st_write(g, paste0("ch3_forGAMs_poly_prefire180_static.gpkg"), delete_dsn = T)

# prefire 90
gref = st_read(paste0("ch3_isochrons_prefire90.gpkg"))
g = g |> 
  filter(ID %in% gref$ID)

st_write(g, paste0("ch3_forGAMs_poly_prefire90_static.gpkg"), delete_dsn = T)

# prefire 60
gref = st_read(paste0("ch3_isochrons_prefire60.gpkg"))
g = g |> 
  filter(ID %in% gref$ID)

st_write(g, paste0("ch3_forGAMs_poly_prefire60_static.gpkg"), delete_dsn = T)

# prefire 30
gref = st_read(paste0("ch3_isochrons_prefire30.gpkg"))
g = g |> 
  filter(ID %in% gref$ID)

st_write(g, paste0("ch3_forGAMs_poly_prefire30_static.gpkg"), delete_dsn = T)

# prefire 14
gref = st_read(paste0("ch3_isochrons_prefire14.gpkg"))
g = g |> 
  filter(ID %in% gref$ID)

st_write(g, paste0("ch3_forGAMs_poly_prefire14_static.gpkg"), delete_dsn = T)

# prefire 7
gref = st_read(paste0("ch3_isochrons_prefire7.gpkg"))
g = g |> 
  filter(ID %in% gref$ID)

st_write(g, paste0("ch3_forGAMs_poly_prefire7_static.gpkg"), delete_dsn = T)

## still need to extract: fire type categories (FESM directly) -> not all isochrons align with FESM fires
## aspect, wind direction done, fire regime types (in order to restrict less representative veg types), number of dwellings, distance to roads and distance to water (take the min of both) done