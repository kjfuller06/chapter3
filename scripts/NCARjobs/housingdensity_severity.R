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
g_buffer = st_buffer(g, dist = 10000) ## create a buffer around each point with a radius of 10km
g_buffer = st_transform(g_buffer, crs = st_crs(houses))

houses = st_intersection(houses, g_buffer)
st_write(houses[1:100,], "houses_severity.gpkg", delete_dsn = T)

st_geometry(houses) = NULL
houses = houses |> 
  filter(startdate < poly_sD) |> 
  dplyr::select(shot_number) |>
  group_by(shot_number) |>
  tally()
houses$house.density = houses$n/(pi * 100) ## calculate number of houses within a circle with radius of 10km
g = g |> 
  left_join(houses) |> 
  dplyr::select(shot_number,
                house.density)
g$house.density[is.na(g$house.density)] = 0
# setwd("E:/chapter3/dwellings/")
st_write(g, "housing_severity_density.gpkg", delete_dsn = T)
