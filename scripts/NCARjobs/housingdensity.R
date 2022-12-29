library(raster)
library(sf)
library(tidyverse)
library(geojsonsf)
library(rjson)
library(rgeos)

# severity ####
setwd("/glade/scratch/kjfuller/data")
# setwd("E:/chapter3/dwellings")
houses = st_read("houses.gpkg")
houses$startdate = as.POSIXct(houses$startdate, 1, 8)
sf::sf_use_s2(FALSE)

setwd("/glade/scratch/kjfuller/data/chapter3")
# setwd("E:/chapter3/for GAMs")
g = st_read("ch3_severity_prefire180.gpkg")
g$poly_sD = as.POSIXct(g$lasttim)
g_buffer = st_buffer(g, dist = 10000) ## create a buffer around each point with a radius of 5km
g_buffer = st_transform(g_buffer, crs = st_crs(houses))

houses = st_intersection(houses, g_buffer)
# st_write(houses, "houses_severity.gpkg", delete_dsn = T)

st_geometry(houses) = NULL
houses = houses |>
  filter(startdate < poly_sD)

house_agg = houses |>
  group_by(shot_number) |>
  tally() |>
  as.data.frame()
houses$house.density = houses$n/(pi * 100) ## calculate number of houses within a circle with radius of 10km
houses = houses |> 
  dplyr::select(shot_number,
                house.density)
g = g |> 
  left_join(houses)
g$house.density[is.na(g$house.density)] = 0
# setwd("E:/chapter3/dwellings/")
st_write(g, "housing_severity_density.gpkg", delete_dsn = T)

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

# setwd("E:/chapter3/dwellings")
houses = st_read("houses.gpkg")
houses$startdate = as.POSIXct(houses$startdate, 1, 8)
sf::sf_use_s2(FALSE)

houses.iso = st_intersection(houses, g)
setwd("/glade/scratch/kjfuller/data/chapter3")
st_write(houses.iso, "houses_isochrons.gpkg", delete_dsn = T)

st_geometry(g) = NULL
st_geometry(houses.iso) = NULL
houses.iso = houses.iso |>
  filter(startdate < poly_sD)

house_agg = houses.iso |>
  group_by(ID) |>
  tally() |>
  as.data.frame()
house_id = houses.iso |>
  dplyr::select(ID, area) |>
  unique()
houses.iso = full_join(house_agg, house_id)
houses.iso$house.density = houses.iso$n/houses.iso$area
g = g |> 
  left_join(houses.iso)
g$house.density[is.na(g$house.density)] = 0
g$n[is.na(g$n)] = 0
# setwd("E:/chapter3/dwellings/")
write.csv(g, "housing_isochrons_density.csv", row.names = F)