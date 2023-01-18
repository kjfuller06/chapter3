library(sf)
library(raster)
library(tidyverse)
library(exactextractr)
library(rgeos)

setwd("/glade/scratch/kjfuller/data/chapter3")
# setwd("E:/chapter3/dwellings")
houses = read.csv("housing_isochrons_density.csv") |> 
  dplyr::select(ID, house.density)

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
                cover,
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
g_agg2 = aggregate(data = g_agg, cover ~ ID, FUN = median)
g_agg3 = aggregate(data = g_agg, cover_z_1 ~ ID, FUN = median)
g_agg4 = aggregate(data = g_agg, over_cover ~ ID, FUN = median)
g_agg5 = aggregate(data = g_agg, fhd_normal ~ ID, FUN = median)

print("joining all median values of GEDI structure to isochron data")
g = g |> 
  right_join(g_agg1) |> 
  right_join(g_agg2) |> 
  right_join(g_agg3) |> 
  right_join(g_agg4) |> 
  right_join(g_agg5)

setwd("/glade/scratch/kjfuller/data/chapter3")
g = st_transform(g, crs = targetcrs)
st_write(g, paste0("ch3_forGAMs_poly_prefire180_structure.gpkg"), delete_dsn = T)
