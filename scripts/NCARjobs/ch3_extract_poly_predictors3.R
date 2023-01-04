library(sf)
library(raster)
library(tidyverse)
library(exactextractr)
library(rgeos)

# # start ####
# setwd("/glade/scratch/kjfuller/data")
# # setwd("E:/chapter3/from Michael")
# wind = st_read("wind_direction.gpkg")
# # remove all directions when windspeed == 0; select just the stations for calculating distances
# wind = wind |> 
#   filter(windspeed != 0)
# 
# # wind median ####
# setwd("/glade/scratch/kjfuller/data/chapter3")
# # setwd("E:/chapter3/for GAMs")
# g = st_read("ch3_forGAMs_poly_prefire180_structure.gpkg")
# targetcrs = st_crs(g)
# g = g |> 
#   dplyr::select(ID,
#                 poly_sD,
#                 poly_eD)
# g_buffer = st_buffer(g, dist = 100000)
# g_buffer = st_transform(g_buffer, st_crs(wind))
# 
# stations = wind |> 
#   dplyr::select(station)
# stations = stations[!duplicated(stations$station),]
# st_geometry(wind) = NULL
# 
# counter <- 0
# l <- list(NULL)
# size <- 1
# windfun = function(x){
#   if( .GlobalEnv$counter == .GlobalEnv$size )
#   {length(.GlobalEnv$l) <- .GlobalEnv$size <- .GlobalEnv$size * 2}
#   
#   tryCatch({
#     # select a buffered polygon and index all stations within 100km
#     g_temp = g_buffer[x,]
#     stat_temp = stations[g_temp,]
#     
#     # select the original polygon and calculate distance from centroid to selected stations
#     g_temp = g |> 
#       filter(ID %in% g_temp$ID)
#     g_center = st_centroid(g_temp)
#     g_center = st_transform(g_center, st_crs(g_buffer))
#     stat_temp$dist = as.numeric(st_distance(g_center, stat_temp))
#     st_geometry(stat_temp) = NULL
#     st_geometry(g_temp) = NULL
#     
#     # select wind data based on polygon timeframe
#     wind_temp = wind |> 
#       filter(station %in% stat_temp$station) |> 
#       left_join(stat_temp)
#     minT = wind_temp |> 
#       filter(DateTime <= min(g_temp$poly_sD))
#     minT = max(minT$DateTime, na.rm = T)
#     maxT = wind_temp |> 
#       filter(DateTime >= max(g_temp$poly_eD))
#     maxT = min(maxT$DateTime, na.rm = T)
#     wind_temp = wind_temp |> 
#       filter(DateTime >= minT) |> 
#       filter(DateTime <= maxT)
#     
#     # calculate the median wind speed within the polygon timeframe at each selected station, join to station data
#     winddir = aggregate(data = wind_temp, winddir ~ station, FUN = median)
#     names(winddir)[2] = "winddir"
#     windsp = aggregate(data = wind_temp, windspeed ~ station, FUN = median)
#     names(windsp)[2] = "windspeed"
#     windgt = aggregate(data = wind_temp, windgust ~ station, FUN = median)
#     names(windgt)[2] = "windgust"
#     
#     wind_var = aggregate(data = wind_temp, winddir ~ station, FUN = sd)
#     names(wind_var)[2] = "wind.stdev"
#     wind_temp = stat_temp |> 
#       inner_join(winddir) |> 
#       inner_join(wind_var) |> 
#       inner_join(windsp) |> 
#       inner_join(windgt)
#     # write number of stations used for calculation
#     g_temp$windstats = nrow(winddir)
#     # create distance-based weights and calculate distance-weighted mean and wind impact index
#     wind_temp$weight = 1 - (wind_temp$dist)/max(wind_temp$dist+1000)
#     g_temp$winddir = weighted.mean(wind_temp$winddir, wind_temp$weight)
#     g_temp$wind.stdev = weighted.mean(wind_temp$wind.stdev, wind_temp$weight)
#     g_temp$windspeed = weighted.mean(wind_temp$windspeed, wind_temp$weight)
#     g_temp$windgust = weighted.mean(wind_temp$windgust, wind_temp$weight)
#     
#     # g_temp$winddiff = g_temp$aspect - g_temp$winddir ## aspect not extracted yet
#     # g_temp$winddiff = cos(g_temp$winddiff * pi / 180)
#     
#     # l[[x]] = g_temp
#     
#     .GlobalEnv$counter <- .GlobalEnv$counter + 1
#     .GlobalEnv$l[[.GlobalEnv$counter]] <- g_temp
#     
#   }, error = function(e){print(x); cat("ERROR :", conditionMessage(e), "\n")})
# }
# for(i in c(1:nrow(g))){
#   windfun(i)
#   print(i)
# }
# print("binding rows")
# g_temp = bind_rows(l)
# print("joining to original isochron data")
# g = left_join(g, g_temp)
# 
# # write to disk ####
# setwd("/glade/scratch/kjfuller/data/chapter3")
# st_write(g, paste0("ch3_forGAMs_poly_prefire180_wind.gpkg"), delete_dsn = T)
# 
# ## still need to extract: fire type categories (FESM directly) -> not all isochrons align with FESM fires
# ## aspect, wind direction done, fire regime types (in order to restrict less representative veg types), number of dwellings, distance to roads and distance to water (take the min of both) done

# # start ####
# setwd("/glade/scratch/kjfuller/data")
# # setwd("E:/chapter3/from Michael")
# wind = st_read("wind_direction.gpkg")
# # remove all directions when windspeed == 0; select just the stations for calculating distances
# wind = wind |>
#   filter(windspeed != 0)
# 
# # wind min and max ####
# setwd("/glade/scratch/kjfuller/data/chapter3")
# # setwd("E:/chapter3/for GAMs")
# g = st_read("ch3_forGAMs_poly_prefire180_wind.gpkg")
# targetcrs = st_crs(g)
# g = g |>
#   dplyr::select(ID,
#                 poly_sD,
#                 poly_eD)
# g_buffer = st_buffer(g, dist = 100000)
# g_buffer = st_transform(g_buffer, st_crs(wind))
# 
# stations = wind |>
#   dplyr::select(station)
# stations = stations[!duplicated(stations$station),]
# st_geometry(wind) = NULL
# 
# counter <- 0
# l <- list(NULL)
# size <- 1
# windfun = function(x){
#   if( .GlobalEnv$counter == .GlobalEnv$size )
#   {length(.GlobalEnv$l) <- .GlobalEnv$size <- .GlobalEnv$size * 2}
# 
#   tryCatch({
#     # select a buffered polygon and index all stations within 100km
#     g_temp = g_buffer[x,]
#     stat_temp = stations[g_temp,]
# 
#     # select the original polygon and calculate distance from centroid to selected stations
#     g_temp = g |>
#       filter(ID %in% g_temp$ID)
#     g_center = st_centroid(g_temp)
#     g_center = st_transform(g_center, st_crs(g_buffer))
#     stat_temp$dist = as.numeric(st_distance(g_center, stat_temp))
#     st_geometry(stat_temp) = NULL
#     st_geometry(g_temp) = NULL
# 
#     # select wind data based on polygon timeframe
#     wind_temp = wind |>
#       filter(station %in% stat_temp$station) |>
#       left_join(stat_temp)
#     minT = wind_temp |>
#       filter(DateTime <= min(g_temp$poly_sD))
#     minT = max(minT$DateTime, na.rm = T)
#     maxT = wind_temp |>
#       filter(DateTime >= max(g_temp$poly_eD))
#     maxT = min(maxT$DateTime, na.rm = T)
#     wind_temp = wind_temp |>
#       filter(DateTime >= minT) |>
#       filter(DateTime <= maxT)
# 
#     # calculate the median wind speed within the polygon timeframe at each selected station, join to station data
#     windsp = aggregate(data = wind_temp, windspeed ~ station, FUN = max)
#     names(windsp)[2] = "windspeed_max"
#     windsp2 = aggregate(data = wind_temp, windspeed ~ station, FUN = min)
#     names(windsp2)[2] = "windspeed_min"
#     windgt = aggregate(data = wind_temp, windgust ~ station, FUN = max)
#     names(windgt)[2] = "windgust_max"
#     
#     wind_temp = stat_temp |>
#       inner_join(windsp) |>
#       inner_join(windsp2) |>
#       inner_join(windgt)
#     # create distance-based weights and calculate distance-weighted mean and wind impact index
#     wind_temp$weight = 1 - (wind_temp$dist)/max(wind_temp$dist+1000)
#     g_temp$windspeed_max = weighted.mean(wind_temp$windspeed_max, wind_temp$weight)
#     g_temp$windspeed_min = weighted.mean(wind_temp$windspeed_min, wind_temp$weight)
#     g_temp$windgust_max = weighted.mean(wind_temp$windgust_max, wind_temp$weight)
#     
#     # l[[x]] = g_temp
# 
#     .GlobalEnv$counter <- .GlobalEnv$counter + 1
#     .GlobalEnv$l[[.GlobalEnv$counter]] <- g_temp
# 
#   }, error = function(e){print(x); cat("ERROR :", conditionMessage(e), "\n")})
# }
# for(i in c(1:nrow(g))){
#   windfun(i)
#   print(i)
# }
# print("binding rows")
# g_temp = bind_rows(l)
# print("joining to original isochron data")
# g = left_join(g, g_temp)
# 
# # write to disk ####
# setwd("/glade/scratch/kjfuller/data/chapter3")
# st_write(g, paste0("ch3_forGAMs_poly_prefire180_wind2.gpkg"), delete_dsn = T)
# 
# ## still need to extract: fire type categories (FESM directly) -> not all isochrons align with FESM fires
# ## aspect, wind direction done, fire regime types (in order to restrict less representative veg types), number of dwellings, distance to roads and distance to water (take the min of both) done
# 

# # start ####
# setwd("/glade/scratch/kjfuller/data")
# # setwd("E:/chapter3/from Michael")
# wind = st_read("wind_direction.gpkg")
# # remove all directions when windspeed == 0; select just the stations for calculating distances
# wind = wind |>
#   filter(windspeed != 0)
# 
# # wind quantiles ####
# setwd("/glade/scratch/kjfuller/data/chapter3")
# # setwd("E:/chapter3/for GAMs")
# g = st_read("ch3_forGAMs_poly_prefire180_wind.gpkg")
# targetcrs = st_crs(g)
# g = g |>
#   dplyr::select(ID,
#                 poly_sD,
#                 poly_eD)
# g_buffer = st_buffer(g, dist = 100000)
# g_buffer = st_transform(g_buffer, st_crs(wind))
# 
# stations = wind |>
#   dplyr::select(station)
# stations = stations[!duplicated(stations$station),]
# st_geometry(wind) = NULL
# 
# counter <- 0
# l <- list(NULL)
# size <- 1
# windfun = function(x){
#   if( .GlobalEnv$counter == .GlobalEnv$size )
#   {length(.GlobalEnv$l) <- .GlobalEnv$size <- .GlobalEnv$size * 2}
# 
#   tryCatch({
#     # select a buffered polygon and index all stations within 100km
#     g_temp = g_buffer[x,]
#     stat_temp = stations[g_temp,]
# 
#     # select the original polygon and calculate distance from centroid to selected stations
#     g_temp = g |>
#       filter(ID %in% g_temp$ID)
#     g_center = st_centroid(g_temp)
#     g_center = st_transform(g_center, st_crs(g_buffer))
#     stat_temp$dist = as.numeric(st_distance(g_center, stat_temp))
#     st_geometry(stat_temp) = NULL
#     st_geometry(g_temp) = NULL
# 
#     # select wind data based on polygon timeframe
#     wind_temp = wind |>
#       filter(station %in% stat_temp$station) |>
#       left_join(stat_temp)
#     minT = wind_temp |>
#       filter(DateTime <= min(g_temp$poly_sD))
#     minT = max(minT$DateTime, na.rm = T)
#     maxT = wind_temp |>
#       filter(DateTime >= max(g_temp$poly_eD))
#     maxT = min(maxT$DateTime, na.rm = T)
#     wind_temp = wind_temp |>
#       filter(DateTime >= minT) |>
#       filter(DateTime <= maxT)
# 
#     # calculate the median wind speed within the polygon timeframe at each selected station, join to station data
#     windsp = aggregate(data = wind_temp, windspeed ~ station, FUN = function(x) quantile(x, probs = 0.1, na.rm = T))
#     names(windsp)[2] = "windspeed.1"
#     windsp2 = aggregate(data = wind_temp, windspeed ~ station, FUN = function(x) quantile(x, probs = 0.9, na.rm = T))
#     names(windsp2)[2] = "windspeed.9"
#     windgt = aggregate(data = wind_temp, windgust ~ station, FUN = function(x) quantile(x, probs = 0.9, na.rm = T))
#     names(windgt)[2] = "windgust.9"
# 
#     wind_temp = stat_temp |>
#       inner_join(windsp) |>
#       inner_join(windsp2) |>
#       inner_join(windgt)
#     # create distance-based weights and calculate distance-weighted mean and wind impact index
#     wind_temp$weight = 1 - (wind_temp$dist)/max(wind_temp$dist+1000)
#     g_temp$windspeed.1 = weighted.mean(wind_temp$windspeed.1, wind_temp$weight)
#     g_temp$windspeed.9 = weighted.mean(wind_temp$windspeed.9, wind_temp$weight)
#     g_temp$windgust.9 = weighted.mean(wind_temp$windgust.9, wind_temp$weight)
# 
#     # l[[x]] = g_temp
# 
#     .GlobalEnv$counter <- .GlobalEnv$counter + 1
#     .GlobalEnv$l[[.GlobalEnv$counter]] <- g_temp
# 
#   }, error = function(e){print(x); cat("ERROR :", conditionMessage(e), "\n")})
# }
# for(i in c(1:nrow(g))){
#   windfun(i)
#   print(i)
# }
# print("binding rows")
# g_temp = bind_rows(l)
# print("joining to original isochron data")
# g = left_join(g, g_temp)
# 
# # write to disk ####
# setwd("/glade/scratch/kjfuller/data/chapter3")
# st_write(g, paste0("ch3_forGAMs_poly_prefire180_wind3.gpkg"), delete_dsn = T)
# 
# ## still need to extract: fire type categories (FESM directly) -> not all isochrons align with FESM fires
# ## aspect, wind direction done, fire regime types (in order to restrict less representative veg types), number of dwellings, distance to roads and distance to water (take the min of both) done
# 
# process all outputs ####
setwd("E:/chapter3/for GAMs")
g1 = st_read("ch3_forGAMs_poly_prefire180_structure.gpkg")
g1$prog = (st_area(g1)/1000000)/g1$progtime

g2 = st_read("ch3_forGAMs_poly_prefire180_elevation.gpkg") |>
  dplyr::select(ID,
                elevation_sd,
                elevation_md)
g2_issue = g2[is.na(g2$elevation_sd),]
st_area(g1[g2$ID %in% g2_issue$ID,])
min(st_area(g1))
## issues result from very small polygons
g2$elevation_sd[is.na(g2$elevation_sd)] = 0
g2 = g2 |>
  dplyr::select(ID,
                elevation_sd)
st_geometry(g2) = NULL

g3 = st_read("ch3_forGAMs_poly_prefire180_fueltype.gpkg") |>
  dplyr::select(ID,
                fueltype,
                fire_reg)

st_geometry(g3) = NULL
summary(g3)

# g4 = st_read("ch3_forGAMs_poly_prefire180_slope2.gpkg") |>
#   dplyr::select(ID,
#                 slope,
#                 slope_max,
#                 slope_min)
# st_geometry(g4) = NULL
# summary(g4)

g5 = st_read("ch3_forGAMs_poly_prefire180_aspect.gpkg")
st_geometry(g5) = NULL
summary(g5)
g6 = st_read("ch3_forGAMs_poly_prefire180_firelines.gpkg")
st_geometry(g6) = NULL
summary(g6)
g7 = st_read("ch3_forGAMs_poly_prefire180_roads.gpkg")
st_geometry(g7) = NULL
summary(g7)
g8 = st_read("ch3_forGAMs_poly_prefire180_water2.gpkg")
st_geometry(g8) = NULL
summary(g8)
g9 = st_read("ch3_forGAMs_poly_prefire180_water3.gpkg")
st_geometry(g9) = NULL
summary(g9)
g10 = st_read("ch3_forGAMs_poly_prefire180_water4.gpkg")
st_geometry(g10) = NULL
summary(g10)

g11 = st_read("ch3_forGAMs_poly_prefire180_bark1.2.gpkg")
st_geometry(g11) = NULL
summary(g11)
g11 = g11 |>
  filter(!is.na(stringybark))
summary(g11)

g12 = st_read("ch3_forGAMs_poly_prefire180_bark2.2.gpkg")
st_geometry(g12) = NULL
g12 = g12 |>
  filter(!is.na(ribbonbark))
summary(g12)

g13 = st_read("ch3_forGAMs_poly_prefire180_LFMC.gpkg")
st_geometry(g13) = NULL
summary(g13)
all(g13$poly_sD > g13$LFMC_sD)
## TRUE
g13 = g13 |>
  dplyr::select(ID,
                LFMC,
                LFMC.1,
                LFMC.9)
summary(g13)

g14 = st_read("ch3_forGAMs_poly_prefire180_dynamic.gpkg")
st_geometry(g14) = NULL
summary(g14)
all(g14$poly_sD > g14$LFMC_sD)
## TRUE
all(g14$poly_sD >= g14$VPD_sD)
## TRUE
g14 = g14 |>
  dplyr::select(ID,
                VPD,
                VPD.1,
                VPD.9)
summary(g14)

g15 = st_read("ch3_forGAMs_poly_prefire180_wind.gpkg")
st_geometry(g15) = NULL
summary(g15)
g15$wind.stdev[is.na(g15$wind.stdev)] = 0
g15 = g15 |>
  filter(!is.na(windspeed)) |>
  dplyr::select(ID,
                winddir,
                wind.stdev,
                windspeed,
                windgust)
summary(g15)

g16 = st_read("ch3_forGAMs_poly_prefire180_wind3.gpkg")
st_geometry(g16) = NULL
summary(g16)
g16 = g16 |>
  filter(!is.na(windspeed.9)) |>
  dplyr::select(ID,
                windspeed.1,
                windspeed.9,
                windgust.9)
summary(g16)

g = g1 |>
  left_join(g2) |>
  left_join(g3) |>
  # left_join(g4) |>
  left_join(g5) |>
  left_join(g6) |>
  left_join(g7) |>
  left_join(g8) |>
  left_join(g9) |>
  left_join(g10) |>
  left_join(g11) |>
  left_join(g12) |>
  left_join(g13) |>
  left_join(g14) |>
  left_join(g15) |>
  left_join(g16) |> 
  filter(!is.na(fire_reg)) |>
  filter(!is.na(LFMC))
rm(g1, g2, g3, 
   # g4, 
   g5, g6, g7, g8, g9, g10, g11, g12, g13, g14, g15, g16)

g$winddiff.iso = cos((g$aspect - g$maxwd) * pi / 180)
g$winddiff.bom = cos((g$aspect - g$winddir) * pi / 180)

# g = g |> 
#   filter(prog < 202)
g$breaks = apply(g |> dplyr::select(firelines, roads), 1, FUN = min, na.rm = T)
g$breaks.all2 = apply(g |> dplyr::select(firelines, roads, water2), 1, FUN = min, na.rm = T)
g$breaks.all3 = apply(g |> dplyr::select(firelines, roads, water2, water3), 1, FUN = min, na.rm = T)
g$breaks.all4 = apply(g |> dplyr::select(firelines, roads, water2, water3, water4), 1, FUN = min, na.rm = T)

g$ffdi_cat = NA
g$ffdi_cat[g$ffdi_final <= 12] = "low"
g$ffdi_cat[g$ffdi_final > 12 & g$ffdi_final <= 25] = "high"
g$ffdi_cat[g$ffdi_final > 25 & g$ffdi_final <= 49] = "very high"
g$ffdi_cat[g$ffdi_final > 49 & g$ffdi_final <= 74] = "severe +"
g$ffdi_cat[g$ffdi_final > 74 & g$ffdi_final <= 99] = "severe +"
g$ffdi_cat[g$ffdi_final > 99] = "severe +"
g$ffdi_cat = factor(g$ffdi_cat, levels = c("low",
                                           "high",
                                           "very high",
                                           "severe +"))

g = g |>
  filter(prog < 1000) |> 
  filter(fire_reg != 7 & fire_reg != 9)

# prefire 180 ####
setwd("E:/chapter3/GEDI_FESM")
gref = st_read("ch3_isochrons_prefire180.gpkg")
st_geometry(gref) = NULL
gref = gref |>
  dplyr::select(ID,
                maxtemp:maxwd,
                ffdi_final) |>
  unique()
length(unique(gref$ID)) == nrow(gref)
## TRUE
gref = inner_join(g, gref)
summary(gref)
nrow(gref)
## 1,727
setwd("E:/chapter3/for GAMs")
st_write(gref, "ch3_forGAMs_poly_prefire180_final.gpkg", delete_dsn = T)

# prefire 90 ####
setwd("E:/chapter3/GEDI_FESM")
gref = st_read("ch3_isochrons_prefire90.gpkg")
st_geometry(gref) = NULL
gref = gref |>
  dplyr::select(ID,
                maxtemp:maxwd,
                ffdi_final) |>
  unique()
length(unique(gref$ID)) == nrow(gref)
## TRUE
gref = inner_join(g, gref)
nrow(gref)
## 806
setwd("E:/chapter3/for GAMs")
st_write(gref, "ch3_forGAMs_poly_prefire90_final.gpkg", delete_dsn = T)

# prefire 60 ####
setwd("E:/chapter3/GEDI_FESM")
gref = st_read("ch3_isochrons_prefire60.gpkg")
st_geometry(gref) = NULL
gref = gref |>
  dplyr::select(ID,
                maxtemp:maxwd,
                ffdi_final) |>
  unique()
length(unique(gref$ID)) == nrow(gref)
## TRUE
gref = inner_join(g, gref)
nrow(gref)
## 543
setwd("E:/chapter3/for GAMs")
st_write(gref, "ch3_forGAMs_poly_prefire60_final.gpkg", delete_dsn = T)

# prefire 30 ####
setwd("E:/chapter3/GEDI_FESM")
gref = st_read("ch3_isochrons_prefire30.gpkg")
st_geometry(gref) = NULL
gref = gref |>
  dplyr::select(ID,
                maxtemp:maxwd,
                ffdi_final) |>
  unique()
length(unique(gref$ID)) == nrow(gref)
## TRUE
gref = inner_join(g, gref)
nrow(gref)
## 161
setwd("E:/chapter3/for GAMs")
st_write(gref, "ch3_forGAMs_poly_prefire30_final.gpkg", delete_dsn = T)

# prefire 14 ####
setwd("E:/chapter3/GEDI_FESM")
gref = st_read("ch3_isochrons_prefire14.gpkg")
st_geometry(gref) = NULL
gref = gref |>
  dplyr::select(ID,
                maxtemp:maxwd,
                ffdi_final) |>
  unique()
length(unique(gref$ID)) == nrow(gref)
## TRUE
gref = inner_join(g, gref)
nrow(gref)
## 32
setwd("E:/chapter3/for GAMs")
st_write(gref, "ch3_forGAMs_poly_prefire14_final.gpkg", delete_dsn = T)

# prefire 7 ####
setwd("E:/chapter3/GEDI_FESM")
gref = st_read("ch3_isochrons_prefire7.gpkg")
st_geometry(gref) = NULL
gref = gref |>
  dplyr::select(ID,
                maxtemp:maxwd,
                ffdi_final) |>
  unique()
length(unique(gref$ID)) == nrow(gref)
## TRUE
gref = inner_join(g, gref)
nrow(gref)
## 14
setwd("E:/chapter3/for GAMs")
st_write(gref, "ch3_forGAMs_poly_prefire7_final.gpkg", delete_dsn = T)
