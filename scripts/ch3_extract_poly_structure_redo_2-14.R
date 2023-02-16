library(sf)
library(raster)
library(tidyverse)
library(exactextractr)
library(rgeos)

# load forest structure data ####
# setwd("/glade/scratch/kjfuller/data/chapter3")
setwd("E:/chapter3/GEDI_FESM/")
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
# setwd("/glade/scratch/kjfuller/data")
setwd("E:/chapter3/isochrons")
g = st_read("isochrons_8days.gpkg")

# calculate and merge structural data ####
g_agg = g |> 
  inner_join(gedi)
# calculate the median of continuous variables
g_agg1 = aggregate(data = g_agg, rh98 ~ ID, FUN = median)
names(g_agg1)[2] = "rh98"
g_agg1.1 = aggregate(data = g_agg, rh98 ~ ID, FUN = function(x) quantile(x, probs = 0.1, na.rm = T))
names(g_agg1.1)[2] = "rh98.1"
g_agg1.9 = aggregate(data = g_agg, rh98 ~ ID, FUN = function(x) quantile(x, probs = 0.9, na.rm = T))
names(g_agg1.9)[2] = "rh98.9"
g_agg2 = aggregate(data = g_agg, cover ~ ID, FUN = median)
names(g_agg2)[2] = "cover"
g_agg2.1 = aggregate(data = g_agg, cover ~ ID, FUN = function(x) quantile(x, probs = 0.1, na.rm = T))
names(g_agg2.1)[2] = "cover.1"
g_agg2.9 = aggregate(data = g_agg, cover ~ ID, FUN = function(x) quantile(x, probs = 0.9, na.rm = T))
names(g_agg2.9)[2] = "cover.9"
g_agg3 = aggregate(data = g_agg, cover_z_1 ~ ID, FUN = median)
names(g_agg3)[2] = "cover_z_1"
g_agg3.1 = aggregate(data = g_agg, cover_z_1 ~ ID, FUN = function(x) quantile(x, probs = 0.1, na.rm = T))
names(g_agg3.1)[2] = "cover_z_1.1"
g_agg3.9 = aggregate(data = g_agg, cover_z_1 ~ ID, FUN = function(x) quantile(x, probs = 0.9, na.rm = T))
names(g_agg3.9)[2] = "cover_z_1.9"
g_agg4 = aggregate(data = g_agg, over_cover ~ ID, FUN = median)
names(g_agg4)[2] = "over_cover"
g_agg4.1 = aggregate(data = g_agg, over_cover ~ ID, FUN = function(x) quantile(x, probs = 0.1, na.rm = T))
names(g_agg4.1)[2] = "over_cover.1"
g_agg4.9 = aggregate(data = g_agg, over_cover ~ ID, FUN = function(x) quantile(x, probs = 0.9, na.rm = T))
names(g_agg4.9)[2] = "over_cover.9"
g_agg5 = aggregate(data = g_agg, fhd_normal ~ ID, FUN = median)
names(g_agg5)[2] = "fhd_normal"
g_agg5.1 = aggregate(data = g_agg, fhd_normal ~ ID, FUN = function(x) quantile(x, probs = 0.1, na.rm = T))
names(g_agg5.1)[2] = "fhd_normal.1"
g_agg5.9 = aggregate(data = g_agg, fhd_normal ~ ID, FUN = function(x) quantile(x, probs = 0.9, na.rm = T))
names(g_agg5.9)[2] = "fhd_normal.9"

print("joining all values of GEDI structure to isochron data")
g = g |> 
  right_join(g_agg1) |> 
  right_join(g_agg1.1) |> 
  right_join(g_agg1.9) |> 
  right_join(g_agg2) |>
  right_join(g_agg2.1) |> 
  right_join(g_agg2.9) |> 
  right_join(g_agg3) |>
  right_join(g_agg3.1) |> 
  right_join(g_agg3.9) |> 
  right_join(g_agg4) |> 
  right_join(g_agg4.1) |> 
  right_join(g_agg4.9) |> 
  right_join(g_agg5) |> 
  right_join(g_agg5.1) |> 
  right_join(g_agg5.9)

# setwd("/glade/scratch/kjfuller/data/chapter3")
setwd("E:/chapter3/for GAMs")
g = st_transform(g, crs = targetcrs)
st_write(g, paste0("ch3_forGAMs_poly_prefire180_structure_redo_2-14.gpkg"), delete_dsn = T)
