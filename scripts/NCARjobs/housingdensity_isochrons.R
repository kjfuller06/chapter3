library(raster)
library(sf)
library(tidyverse)
library(geojsonsf)
library(rjson)
library(rgeos)

# isochrons ####
setwd("/glade/scratch/kjfuller/data")
# setwd("E:/chapter3/isochrons/Progall_ffdi_v3")
g = st_read("progall_ffdi_v3.shp")
targetcrs = st_crs(g)
g$ID = c(1:nrow(g))
g$poly_sD = as.POSIXct(g$lasttim, format = "%Y-%m-%d %H:%M")
g$poly_eD = as.POSIXct(g$time, format = "%Y-%m-%d %H:%M")
g$progtime = difftime(g$poly_eD, g$poly_sD, units = "hours")
g$progtime = as.numeric(g$progtime)
g = g |>
  filter(progtime > 0) |>
  filter(progtime <= 192) ## progression time < 8 days (8 * 24 = 192)
g = g |>
  dplyr::select(ID, poly_sD, area)

# # setwd("E:/chapter3/dwellings")
# houses = st_read("houses.gpkg")
# houses$startdate = as.POSIXct(houses$startdate, 1, 8)
# sf::sf_use_s2(FALSE)
# 
# houses.iso = st_intersection(houses, g)
# st_write(houses.iso, "houses_isochrons.gpkg", delete_dsn = T)
houses.iso = st_read("houses_isochrons.gpkg")

st_geometry(g) = NULL
st_geometry(houses.iso) = NULL
houses_agg = houses.iso |>
  filter(startdate < poly_sD) |>
  group_by(ID) |>
  tally() |>
  as.data.frame()
g = g |>
  dplyr::select(ID, area) |>
  unique()
houses.iso = full_join(houses_agg, g)
houses.iso$house.density = houses.iso$n/houses.iso$area
houses.iso$house.density[is.na(houses.iso$house.density)] = 0
g = houses.iso |> 
  dplyr::select(ID, house.density)
setwd("/glade/scratch/kjfuller/data/chapter3")
# setwd("E:/chapter3/dwellings/")
write.csv(g, "housing_isochrons_density.csv", row.names = F)