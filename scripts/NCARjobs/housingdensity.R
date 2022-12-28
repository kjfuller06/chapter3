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
g = g |>
  dplyr::select(ID, poly_sD, area)

# setwd("E:/chapter3/dwellings")
houses = st_read("houses.gpkg")
houses$startdate = as.POSIXct(houses$startdate, 1, 8)
sf::sf_use_s2(FALSE)

houses = st_intersection(houses, g)
st_write(houses, "houses_isochrons.gpkg", delete_dsn = T)

st_geometry(g) = NULL
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
g = g |> 
  left_join(houses)
g$house.density[is.na(g$house.density)] = 0
g$n[is.na(g$n)] = 0
# setwd("E:/chapter3/dwellings/")
write.csv(g, "housing_density.csv", row.names = F)
