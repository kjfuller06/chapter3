library(raster)
library(sf)
library(tidyverse)
library(geojsonsf)
library(rjson)
library(rgeos)

# housing density ####
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

houses = st_read("houses.gpkg")
houses$startdate = as.POSIXct(houses$startdate, 1, 8)
sf::sf_use_s2(FALSE)

houses = st_intersection(houses, g)
st_write(houses, "houses_isochrons.gpkg", delete_dsn = T)

st_geometry(houses) = NULL
houses = houses |>
  filter(startdate < poly_sD)

house_agg = houses |>
  group_by(ID) |>
  tally() |>
  as.data.frame()
house_id = houses |>
  dplyr::select(ID, area) |>
  unique()
houses = full_join(house_agg, house_id)
houses$house.density = houses$n/houses$area
write.csv(houses, "housing_density.csv", row.names = F)