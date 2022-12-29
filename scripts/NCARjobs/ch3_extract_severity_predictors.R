library(sf)
library(raster)
library(tidyverse)
library(exactextractr)

# # # load files ####
# setwd("/glade/scratch/kjfuller/data")
# houses = read.csv("housing_density.csv") |>
#   dplyr::select(ID, house.density)
# # setwd("E:/chapter3/from Michael")
# wind = st_read("wind_direction.gpkg")
# types = read.csv("FESM_firetypes.csv") |>
#   dplyr::select(fire_id, category)
# types$fire_id = as.factor(types$fire_id)
# 
# # setwd("E:/chapter3")
# # types = read.csv("FESM_firetypes.csv")
# # types$fire_id = as.factor(types$fire_id)
# 
# setwd("/glade/scratch/kjfuller/data/LFMC")
# # setwd("E:/chapter3/from Rachael/R project/LFMC/LFMC/GEE_LFMC/LFMC_rasters/GEE_v20May2020_RevisedMask/")
# lfmc = list.files()
# lfmc = data.frame(files = lfmc)
# lfmc$date = as.Date(substr(lfmc$files, 6, 15), format = "%Y_%m_%d")
# 
# setwd("/glade/scratch/kjfuller/data/VPD")
# # setwd("E:/chapter3/from Rachael/VPD")
# vpd = list.files()
# vpd = data.frame(files = vpd)
# vpd$date = as.Date(substr(vpd$files, 5, 12), format = "%Y%m%d")
# 
# # setwd("E:/chapter3/GEDI_FESM")
# # extractfun = function(x){
#   # done #####
#   g = st_read(paste0("ch3_severity_prefire", x, ".gpkg"))
#   targetcrs = st_crs(g)
#   g = g |>
#     filter(fire_sD > "2019-08-04")
#   g$poly_sD = as.POSIXct(g$lasttim, format = "%Y-%m-%d %H:%M")
#   g$poly_eD = as.POSIXct(g$time, format = "%Y-%m-%d %H:%M")
#   g$progtime = as.numeric(difftime(g$time, g$lasttim, units = "hours"))
#   g = g |>
#     filter(progtime <= 192)
# 
#   # load rasters
#   setwd("/glade/scratch/kjfuller/data")
#   # setwd("D:/chapter1/other_data/Final/terrain variables")
# 
#   # elevation
#   r = raster("proj_dem_s.tif")
# 
#   g = st_transform(g, crs = st_crs(r))
#   g_buffer = st_buffer(g, dist = 12.5)
# 
#   g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
#   names(g_temp)[names(g_temp) == "mode"] = "elevation"
#   g = left_join(g, g_temp)
# 
#   # northness
#   r = raster("proj_dem_northness_30m.tif")
# 
#   g = st_transform(g, crs = st_crs(r))
#   g_buffer = st_buffer(g, dist = 12.5)
# 
#   g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
#   names(g_temp)[names(g_temp) == "mode"] = "northness"
#   g = left_join(g, g_temp)
# 
#   # eastness
#   r = raster("proj_dem_eastness_30m.tif")
# 
#   g = st_transform(g, crs = st_crs(r))
#   g_buffer = st_buffer(g, dist = 12.5)
# 
#   g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
#   names(g_temp)[names(g_temp) == "mode"] = "eastness"
#   g = left_join(g, g_temp)
# 
#   # bark types
#   r = raster("NSW_stringybark_distribution.tif")
# 
#   g = st_transform(g, crs = st_crs(r))
#   g_buffer = st_buffer(g, dist = 12.5)
# 
#   g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
#   names(g_temp)[names(g_temp) == "mode"] = "stringybark"
#   g = left_join(g, g_temp)
# 
#   r = raster("NSW_ribboning_distribution.tif")
# 
#   g = st_transform(g, crs = st_crs(r))
#   g_buffer = st_buffer(g, dist = 12.5)
# 
#   g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
#   names(g_temp)[names(g_temp) == "mode"] = "ribbonbark"
#   g = left_join(g, g_temp)
# 
#   setwd("/glade/scratch/kjfuller/data/chapter3")
#   # setwd("E:/chapter3/for GAMs")
#   g = st_transform(g, crs = targetcrs)
#   st_write(g, paste0("ch3_forGAMs_prefire", x, "_static.gpkg"), delete_dsn = T)
#   # g = st_read(paste0("ch3_forGAMs_prefire", x, "_static.gpkg"))
# 
#   # LFMC
#   setwd("/glade/scratch/kjfuller/data/LFMC")
#   setwd("E:/chapter3/from Rachael/R project/LFMC/LFMC/GEE_LFMC/LFMC_rasters/GEE_v20May2020_RevisedMask/")
#   r = raster(lfmc$files[1])
#   g = st_transform(g, crs = st_crs(r))
#   ids = unique(g$ID)
#   l = list()
#   for(i in c(1:length(ids))){
#     g_temp = g |>
#       filter(ID == ids[i])
# 
#     ## select LFMC values that were calculated for dates before the fire started
#     lfmc_temp = lfmc |>
#       filter(as.numeric(difftime(min(g_temp$poly_sD), date)) > 0)
#     ## select the last possible pre-fire LFMC value
#     lfmc_temp = lfmc_temp |>
#       filter(date == max(lfmc_temp$date))
# 
#     r = raster(lfmc_temp$files[1])
# 
#     g_buffer = st_buffer(g_temp, dist = 12.5)
# 
#     g_temp2 = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
#     names(g_temp2)[names(g_temp2) == "mode"] = "LFMC"
#     g_temp2$LFMC_sD = lfmc_temp$date[1]
#     g_temp = left_join(g_temp, g_temp2)
# 
#     ## if any LFMC values are NA, extract the next earliest LFMC value (not iterated)
#     if(any(is.na(g_temp$LFMC))){
#       g_temp2 = g_temp |>
#         filter(is.na(LFMC))
#       if(g_temp2$LFMC_sD != min(lfmc$date)){
#         lfmc_temp = lfmc |>
#           filter(date < g_temp2$LFMC_sD[1])
#         lfmc_temp = lfmc_temp |>
#           filter(date == max(lfmc_temp$date))
# 
#         r = raster(lfmc_temp$files[1])
# 
#         g_buffer = st_buffer(g_temp2, dist = 12.5) |>
#           dplyr::select(-LFMC)
# 
#         g_temp3 = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
#         names(g_temp3)[names(g_temp3) == "mode"] = "LFMC"
#         g_temp2 = left_join(g_temp2, g_temp3)
#         g_temp2$LFMC_sD = lfmc_temp$date[1]
# 
#         g_temp = g_temp |>
#           filter(!is.na(LFMC)) |>
#           rbind(g_temp2)
#       }
#     }
# 
#     l[[i]] = g_temp
#     print(paste0("LFMC ", i))
#   }
#   g_temp = bind_rows(l)
#   st_geometry(g_temp) = NULL
#   g = full_join(g, g_temp)
# 
#   # VPD
#   setwd("/glade/scratch/kjfuller/data/VPD")
#   # setwd("E:/chapter3/from Rachael/VPD")
#   r = raster(vpd$files[1])
#   g = st_transform(g, crs = st_crs(4326))
#   ids = unique(g$ID)
#   l = list()
#   for(i in c(1:length(ids))){
#     g_temp = g |>
#       filter(ID == ids[i])
# 
#     ## select VPD values that were calculated for dates after the fire started; for the fire polygon for each fire, extract values for the previous 24 hours
#       vpd_temp = vpd |>
#         filter(as.numeric(difftime(min(g_temp$poly_sD), date)) <= 0)
#     vpd_temp = vpd_temp |>
#       filter(as.numeric(difftime(min(g_temp$poly_eD), date)) >= 0)
# 
#     r = terra::rast(vpd_temp$files)
# 
#     g_buffer = st_buffer(g_temp, dist = 12.5)
# 
#     g_temp2 = exact_extract(r, g_buffer, fun = "mode", append_cols = F)
#     g_temp2$shot_number = g_temp$shot_number
#     g_temp2 = g_temp2 |>
#       pivot_longer(cols = contains("mode"), values_to = "VPD")
#     g_temp2 = aggregate(data = g_temp2, VPD ~ shot_number, FUN = median)
# 
#     g_temp2$VPD_sD = vpd_temp$date[1]
#     g_temp = left_join(g_temp, g_temp2)
# 
#     l[[i]] = g_temp
#     print(paste0("VPD ", i))
#   }
#   g_temp = bind_rows(l)
#   st_geometry(g_temp) = NULL
#   g = full_join(g, g_temp)
# 
#   g = st_transform(g, crs = targetcrs)
#   setwd("/glade/scratch/kjfuller/data/chapter3")
#   st_write(g, paste0("ch3_forGAMs_prefire", x, "_allvars.gpkg"), delete_dsn = T)
# # #   
# #   # # merge housing density, distance to breaks and fire types ####
# #   # # setwd("E:/chapter3/for GAMs")
# #   # setwd("/glade/scratch/kjfuller/data/chapter3")
# #   # g = st_read(paste0("ch3_forGAMs_prefire", x, "_allvars.gpkg"))
# #   # targetcrs = st_crs(g)
# #   # g$fire_id = as.factor(g$fire_id)
# #   # g = g |> 
# #   #   left_join(houses) |> 
# #   #   left_join(types)
# #   # 
# #   # # aspect ####
# #   # setwd("/glade/scratch/kjfuller/data")
# #   # r = raster("proj_dem_aspect_30m.tif")
# #   # 
# #   # g = st_transform(g, crs = st_crs(r))
# #   # g_buffer = st_buffer(g, dist = 12.5)
# #   # 
# #   # g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
# #   # names(g_temp)[names(g_temp) == "mode"] = "aspect"
# #   # g = left_join(g, g_temp)
# #   # 
# #   # # distance to firelines ####
# #   # r = raster("distancetofirelines_parallel.tif")
# #   # 
# #   # g = st_transform(g, crs = st_crs(r))
# #   # g_buffer = st_buffer(g, dist = 12.5)
# #   # 
# #   # g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
# #   # names(g_temp)[names(g_temp) == "mode"] = "firelines"
# #   # g = left_join(g, g_temp)
# #   # 
# #   # # distance to roads ####
# #   # r = raster("distancetoroads.tif")
# #   # 
# #   # g = st_transform(g, crs = st_crs(r))
# #   # g_buffer = st_buffer(g, dist = 12.5)
# #   # 
# #   # g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
# #   # names(g_temp)[names(g_temp) == "mode"] = "roads"
# #   # g = left_join(g, g_temp)
# #   # 
# #   # # distance to waterways ####
# #   # r = raster("distancetowater_relevance2.tif")
# #   # 
# #   # g = st_transform(g, crs = st_crs(r))
# #   # g_buffer = st_buffer(g, dist = 12.5)
# #   # 
# #   # g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
# #   # names(g_temp)[names(g_temp) == "mode"] = "water2"
# #   # g = left_join(g, g_temp)
# #   # 
# #   # r = raster("distancetowater_relevance3.tif")
# #   # 
# #   # g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
# #   # names(g_temp)[names(g_temp) == "mode"] = "water3"
# #   # g = left_join(g, g_temp)
# #   # 
# #   # r = raster("distancetowater_relevance4.tif")
# #   # 
# #   # g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
# #   # names(g_temp)[names(g_temp) == "mode"] = "water4"
# #   # g = left_join(g, g_temp)
# #   # 
# #   # # r = raster("distancetowater_relevance5.tif")
# #   # # 
# #   # # g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
# #   # # names(g_temp)[names(g_temp) == "mode"] = "water5"
# #   # # g = left_join(g, g_temp)
# #   # # 
# #   # # r = raster("distancetowater_relevance6.tif")
# #   # # 
# #   # # g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
# #   # # names(g_temp)[names(g_temp) == "mode"] = "water6"
# #   # # g = left_join(g, g_temp)
# 
# # wind direction ####
#   g_buffer = st_buffer(g, dist = 100000)
#   g_buffer = st_transform(g_buffer, st_crs(wind))
#   g = st_transform(g, st_crs(wind))
# 
#   # remove all directions when windspeed == 0; select just the stations for calculating distances
#   wind = wind |>
#     filter(windspeed != 0)
# 
#   stations = wind |>
#     dplyr::select(station)
#   stations = stations[!duplicated(stations$station),]
#   st_geometry(wind) = NULL
# 
#   l = list()
#   for(i in c(1:nrow(g))){
#     # select a buffered shot and index all stations within 100km
#     g_temp = g_buffer[i,]
#     stat_temp = stations[g_temp,]
# 
#     # select the original shot and calculate distance to selected stations
#     g_temp = g |>
#       filter(shot_number %in% g_temp$shot_number)
#     stat_temp$dist = as.numeric(st_distance(g_temp, stat_temp))
#     st_geometry(stat_temp) = NULL
#     st_geometry(g_temp) = NULL
# 
#     # if the polygon is the first, overwrite poly_sD as 24hrs before poly_eD (in minutes)
#     if(min(g_temp$poly_sD) == max(g_temp$poly_eD)){
#       g_temp$poly_sD = g_temp$poly_eD - 86400
#     }
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
#     windstats = nrow(winddir)
#     windsp = aggregate(data = wind_temp, windspeed ~ station, FUN = median)
#     names(windsp)[2] = "windspeed"
#     windgt = aggregate(data = wind_temp, windgust ~ station, FUN = median)
#     names(windgt)[2] = "windgust"
# 
#     wind_temp = stat_temp |>
#       inner_join(winddir) |>
#       inner_join(windsp) |>
#       inner_join(windgt)
#     # write number of stations used for calculation
#     wind_temp$windstats = windstats
#     # create distance-based weights and calculate distance-weighted mean and wind impact index
#     wind_temp$weight = 1 - (wind_temp$dist)/max(wind_temp$dist)
#     g_temp$winddir = weighted.mean(wind_temp$winddir, wind_temp$weight)
#     g_temp$windspeed = weighted.mean(wind_temp$windspeed, wind_temp$weight)
#     g_temp$windgust = weighted.mean(wind_temp$windgust, wind_temp$weight)
# 
#     g_temp$winddiff = g_temp$aspect - g_temp$winddir
#     g_temp$winddiff = cos(g_temp$winddiff * pi / 180)
#     l[[i]] = g_temp
#     print(i)
#   }
#   g_temp = bind_rows(l)
#   g = full_join(g, g_temp)
# 
#   setwd("/glade/scratch/kjfuller/data/chapter3")
#   st_write(g, paste0("ch3_forGAMs_prefire", x, "_allvars2.gpkg"), delete_dsn = T)
# }
# 
# extractfun(7)
# extractfun(14)
# extractfun(30)
# extractfun(60)
# extractfun(90)
# extractfun(180)

# # extract just 180 ####
# setwd("/glade/scratch/kjfuller/data")
# # # setwd("E:/chapter3/from Michael")
# wind = st_read("wind_direction.gpkg")
# # types = read.csv("FESM_firetypes.csv") |>
# #   dplyr::select(fire_id, category)
# # types$fire_id = as.factor(types$fire_id)
# # 
# # # merge housing density, distance to breaks and fire types
# # # setwd("E:/chapter3/for GAMs")
# # setwd("/glade/scratch/kjfuller/data/chapter3")
# # g = st_read(paste0("ch3_forGAMs_prefire180_allvars.gpkg"))
# # targetcrs = st_crs(g)
# # g$fire_id = as.factor(g$fire_id)
# # g = g |>
# #   left_join(types)
# # 
# # # aspect
# # setwd("/glade/scratch/kjfuller/data")
# # r = raster("proj_dem_aspect_30m.tif")
# # 
# # g = st_transform(g, crs = st_crs(r))
# # g_buffer = st_buffer(g, dist = 12.5)
# # 
# # g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
# # names(g_temp)[names(g_temp) == "mode"] = "aspect"
# # g = left_join(g, g_temp)
# # 
# # # distance to firelines
# # r = raster("distancetofirelines_parallel.tif")
# # 
# # g = st_transform(g, crs = st_crs(r))
# # g_buffer = st_buffer(g, dist = 12.5)
# # 
# # g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
# # names(g_temp)[names(g_temp) == "mode"] = "firelines"
# # g = left_join(g, g_temp)
# # 
# # # distance to roads
# # r = raster("distancetoroads.tif")
# # 
# # g = st_transform(g, crs = st_crs(r))
# # g_buffer = st_buffer(g, dist = 12.5)
# # 
# # g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
# # names(g_temp)[names(g_temp) == "mode"] = "roads"
# # g = left_join(g, g_temp)
# # 
# # # distance to waterways
# # r = raster("distancetowater_relevance2.tif")
# # 
# # g = st_transform(g, crs = st_crs(r))
# # g_buffer = st_buffer(g, dist = 12.5)
# # 
# # g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
# # names(g_temp)[names(g_temp) == "mode"] = "water2"
# # g = left_join(g, g_temp)
# # 
# # r = raster("distancetowater_relevance3.tif")
# # 
# # g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
# # names(g_temp)[names(g_temp) == "mode"] = "water3"
# # g = left_join(g, g_temp)
# # 
# # r = raster("distancetowater_relevance4.tif")
# # 
# # g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
# # names(g_temp)[names(g_temp) == "mode"] = "water4"
# # g = left_join(g, g_temp)
# # 
# # setwd("/glade/scratch/kjfuller/data/chapter3")
# # st_write(g, paste0("ch3_forGAMs_prefire180_dynamic.gpkg"), delete_dsn = T)
# 
# setwd("/glade/scratch/kjfuller/data/chapter3")
# # setwd("E:/chapter3/for GAMS")
# g_ref = st_read(paste0("ch3_forGAMs_prefire90_allvars2.gpkg"))
# targetcrs = st_crs(g_ref)
# 
# g = st_read(paste0("ch3_forGAMs_prefire180_dynamic.gpkg"))
# g$poly_sD = as.POSIXct(g$lasttim, format = "%Y-%m-%d %H:%M")
# g$poly_eD = as.POSIXct(g$time, format = "%Y-%m-%d %H:%M")
# print(paste0("nrow(g) before filtering == ", nrow(g)))
# 
# # remove all shots that have already been processed, to save time
# g = g |>
#   filter(!shot_number %in% g_ref$shot_number)
# print(paste0("nrow(g) after filtering == ", nrow(g)))
# 
# g_buffer = st_buffer(g, dist = 100000)
# g_buffer = st_transform(g_buffer, st_crs(wind))
# g = st_transform(g, st_crs(wind))
# 
# # remove all directions when windspeed == 0; select just the stations for calculating distances
# wind = wind |>
#   filter(windspeed != 0)
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
#   tryCatch({
#     if( .GlobalEnv$counter == .GlobalEnv$size )
#     {length(.GlobalEnv$l) <- .GlobalEnv$size <- .GlobalEnv$size * 2}
#     
#     # select a buffered shot and index all stations within 100km
#     g_temp = g_buffer[x,]
#     stat_temp = stations[g_temp,]
#     
#     # select the original shot and calculate distance to selected stations
#     g_temp = g |>
#       filter(shot_number %in% g_temp$shot_number)
#     stat_temp$dist = as.numeric(st_distance(g_temp, stat_temp))
#     st_geometry(stat_temp) = NULL
#     st_geometry(g_temp) = NULL
#     
#     # if the polygon is the first, overwrite poly_sD as 24hrs before poly_eD (in minutes)
#     if(min(g_temp$poly_sD) == max(g_temp$poly_eD)){
#       g_temp$poly_sD = g_temp$poly_eD - 86400
#     }
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
#     windstats = nrow(winddir)
#     windsp = aggregate(data = wind_temp, windspeed ~ station, FUN = median)
#     names(windsp)[2] = "windspeed"
#     if(any(!is.na(wind_temp$windgust))){
#       windgt = aggregate(data = wind_temp, windgust ~ station, FUN = median)
#       names(windgt)[2] = "windgust"
#     } else {
#       windgt = windsp
#       names(windgt)[2] = "windgust"
#       windgt$windgust = NA
#     }
#     
#     wind_temp = stat_temp |>
#       inner_join(winddir) |>
#       inner_join(windsp) |>
#       inner_join(windgt)
#     # write number of stations used for calculation
#     wind_temp$windstats = windstats
#     # create distance-based weights and calculate distance-weighted mean and wind impact index
#     wind_temp$weight = 1 - (wind_temp$dist)/max(wind_temp$dist)
#     g_temp$winddir = weighted.mean(wind_temp$winddir, wind_temp$weight)
#     g_temp$windspeed = weighted.mean(wind_temp$windspeed, wind_temp$weight)
#     g_temp$windgust = weighted.mean(wind_temp$windgust, wind_temp$weight)
#     
#     g_temp$winddiff = g_temp$aspect - g_temp$winddir
#     g_temp$winddiff = cos(g_temp$winddiff * pi / 180)
#     # l[[x]] = g_temp
#     
#     .GlobalEnv$counter <- .GlobalEnv$counter + 1
#     .GlobalEnv$l[[.GlobalEnv$counter]] <- g_temp
#   }, error = function(e){print(x); cat("ERROR :", conditionMessage(e), "\n")})
# }
# for(i in c(1:nrow(g))){
#   windfun(i)
#   print(i)
# }
# g_temp = bind_rows(l)
# g = left_join(g, g_temp)
# g = st_transform(g, crs = targetcrs)
# 
# setwd("/glade/scratch/kjfuller/data/chapter3")
# st_write(g, paste0("ch3_forGAMs_prefire180_allvars2.gpkg"), delete_dsn = T)
# 
# clean up outputs ####
setwd("E:/chapter3/dwellings")
housing = st_read("housing_severity_density.gpkg")
st_geometry(housing) = NULL

setwd("E:/chapter3/for GAMs")
gref = st_read(paste0("ch3_severity_prefire7.gpkg"))
targetcrs = st_crs(gref)

# prefire 7
g1 = st_read("ch3_forGAMs_prefire7_static.gpkg")
g2 = st_read("ch3_forGAMs_prefire7_allvars.gpkg")
g3 = st_read("ch3_forGAMs_prefire7_allvars2.gpkg")
lookup = nrow(g3)
st_geometry(g3) = NULL
wind = g3 |>
  dplyr::select(shot_number,
                winddir:winddiff) |>
  filter(!is.na(winddir))
nrow(wind) == lookup/2
## TRUE
others = g3 |>
  dplyr::select(shot_number,
                category:water4) |>
  filter(!is.na(water2))
nrow(others) == lookup/2
## TRUE
g2 = left_join(g2, wind) |>
  left_join(others) |>
  left_join(housing) |> 
  dplyr::select(shot_number,
                veg,
                fire_reg,
                rh98:over_cover,
                slope,
                severity,
                maxtemp:maxwd,
                ffdi_final,
                ID,
                # elevation,
                stringybark,
                ribbonbark,
                LFMC,
                VPD,
                winddir:winddiff,
                category,
                aspect,
                firelines:water4,
                house.density) |> 
  filter(!is.na(LFMC))
g2$ribbonbark[is.na(g2$ribbonbark)] = 0
nrow(g2)
## 30
st_write(g2, "ch3_forGAMs_prefire7_final.gpkg", delete_dsn = T)

# prefire 14
g1 = st_read("ch3_forGAMs_prefire14_static.gpkg")
g2 = st_read("ch3_forGAMs_prefire14_allvars.gpkg")
g3 = st_read("ch3_forGAMs_prefire14_allvars2.gpkg")
lookup = nrow(g3)
st_geometry(g3) = NULL
wind = g3 |>
  dplyr::select(shot_number,
                winddir:winddiff) |>
  filter(!is.na(winddir))
nrow(wind) == lookup/2
## TRUE
others = g3 |>
  dplyr::select(shot_number,
                category:water4) |>
  filter(!is.na(water2))
nrow(others) == lookup/2
## TRUE
g2 = left_join(g2, wind) |>
  left_join(others) |>
  left_join(housing) |> 
  dplyr::select(shot_number,
                veg,
                fire_reg,
                rh98:over_cover,
                slope,
                severity,
                maxtemp:maxwd,
                ffdi_final,
                ID,
                # elevation,
                stringybark,
                ribbonbark,
                LFMC,
                VPD,
                winddir:winddiff,
                category,
                aspect,
                firelines:water4,
                house.density) |> 
  filter(!is.na(LFMC))
g2$ribbonbark[is.na(g2$ribbonbark)] = 0
nrow(g2)
## 331
st_write(g2, "ch3_forGAMs_prefire14_final.gpkg", delete_dsn = T)

# prefire 30
g1 = st_read("ch3_forGAMs_prefire30_static.gpkg")
g2 = st_read("ch3_forGAMs_prefire30_allvars.gpkg")
g3 = st_read("ch3_forGAMs_prefire30_allvars2.gpkg")
lookup = nrow(g3)
st_geometry(g3) = NULL
wind = g3 |>
  dplyr::select(shot_number,
                winddir:winddiff) |>
  filter(!is.na(winddir))
nrow(wind) == lookup/2
## TRUE
others = g3 |>
  dplyr::select(shot_number,
                category:water4) |>
  filter(!is.na(water2))
nrow(others) == lookup/2
## TRUE
g2 = left_join(g2, wind) |>
  left_join(others) |>
  left_join(housing) |> 
  dplyr::select(shot_number,
                veg,
                fire_reg,
                rh98:over_cover,
                slope,
                severity,
                maxtemp:maxwd,
                ffdi_final,
                ID,
                # elevation,
                stringybark,
                ribbonbark,
                LFMC,
                VPD,
                winddir:winddiff,
                category,
                aspect,
                firelines:water4,
                house.density) |> 
  filter(!is.na(LFMC))
g2$ribbonbark[is.na(g2$ribbonbark)] = 0
nrow(g2)
## 3500
st_write(g2, "ch3_forGAMs_prefire30_final.gpkg", delete_dsn = T)

# prefire 60
g1 = st_read("ch3_forGAMs_prefire60_static.gpkg")
g2 = st_read("ch3_forGAMs_prefire60_allvars.gpkg")
g3 = st_read("ch3_forGAMs_prefire60_allvars2.gpkg")
lookup = nrow(g3)
st_geometry(g3) = NULL
wind = g3 |>
  dplyr::select(shot_number,
                winddir:winddiff) |>
  filter(!is.na(winddir))
nrow(wind) == lookup/2
## TRUE
others = g3 |>
  dplyr::select(shot_number,
                category:water4) |>
  filter(!is.na(water2))
nrow(others) == lookup/2
## TRUE
g2 = left_join(g2, wind) |>
  left_join(others) |>
  left_join(housing) |> 
  dplyr::select(shot_number,
                veg,
                fire_reg,
                rh98:over_cover,
                slope,
                severity,
                maxtemp:maxwd,
                ffdi_final,
                ID,
                # elevation,
                stringybark,
                ribbonbark,
                LFMC,
                VPD,
                winddir:winddiff,
                category,
                aspect,
                firelines:water4,
                house.density) |> 
  filter(!is.na(LFMC))
g2$ribbonbark[is.na(g2$ribbonbark)] = 0
nrow(g2)
## 10,696
st_write(g2, "ch3_forGAMs_prefire60_final.gpkg", delete_dsn = T)

# prefire 90
g1 = st_read("ch3_forGAMs_prefire90_static.gpkg")
g2 = st_read("ch3_forGAMs_prefire90_allvars.gpkg")
g3 = st_read("ch3_forGAMs_prefire90_allvars2.gpkg")
lookup = nrow(g3)
st_geometry(g3) = NULL
wind = g3 |>
  dplyr::select(shot_number,
                winddir:winddiff) |>
  filter(!is.na(winddir))
nrow(wind) == lookup/2
## FALSE
nrow(wind) - lookup/2
## -3

wind = g3 |>
  dplyr::select(shot_number,
                winddir:winddiff)
wind = wind[duplicated(wind$shot_number),]
nrow(wind) == lookup/2
## TRUE
sum(is.na(wind$winddir))
## 3

others = g3 |>
  dplyr::select(shot_number,
                category:water4) |>
  filter(!is.na(water2))
nrow(others) == lookup/2
## TRUE
g2 = left_join(g2, wind) |>
  left_join(others) |>
  left_join(housing) |> 
  dplyr::select(shot_number,
                veg,
                fire_reg,
                rh98:over_cover,
                slope,
                severity,
                maxtemp:maxwd,
                ffdi_final,
                ID,
                # elevation,
                stringybark,
                ribbonbark,
                LFMC,
                VPD,
                winddir:winddiff,
                category,
                aspect,
                firelines:water4,
                house.density) |> 
  filter(!is.na(LFMC))
g2$ribbonbark[is.na(g2$ribbonbark)] = 0
nrow(g2)
## 16,540
## 6,591 missing LFMC values

st_write(g2, "ch3_forGAMs_prefire90_final.gpkg", delete_dsn = T)

# prefire 180
g1 = st_read("ch3_forGAMs_prefire180_static.gpkg") ## 62 vars, 52,462 rows
g2 = st_read("ch3_forGAMs_prefire180_allvars.gpkg") ## 66 vars, 52,462 rows
g3 = st_read("ch3_forGAMs_prefire180_dynamic.gpkg") ## 74 vars, 52,462 rows
g4 = st_read("ch3_forGAMs_prefire180_allvars2.gpkg") ## 78 vars, 29,334 rows
lookup = nrow(g3 |> filter(shot_number %in% g4$shot_number))
lookup2 = nrow(g3)
st_geometry(g4) = NULL
wind = g4 |>
  dplyr::select(shot_number,
                winddir:winddiff)
nrow(wind) == lookup
## TRUE

gref = st_read("ch3_forGAMs_prefire90_final.gpkg")
any(gref$shot_number %in% wind$shot_number)
## FALSE
st_geometry(gref) = NULL
wind2 = gref |> 
  dplyr::select(shot_number,
                winddir:winddiff)

g3.1 = right_join(g3, wind) |>
  dplyr::select(-house.density) |>
  left_join(housing) |> 
  dplyr::select(shot_number,
                veg,
                fire_reg,
                rh98:over_cover,
                slope,
                severity,
                maxtemp:maxwd,
                ffdi_final,
                ID,
                # elevation,
                stringybark,
                ribbonbark,
                LFMC,
                VPD,
                winddir:winddiff,
                category,
                aspect,
                firelines:water4,
                house.density) |> 
  filter(!is.na(LFMC))

g3.2 = right_join(g3, wind2) |>
  dplyr::select(-house.density) |>
  left_join(housing) |> 
  dplyr::select(shot_number,
                veg,
                fire_reg,
                rh98:over_cover,
                slope,
                severity,
                maxtemp:maxwd,
                ffdi_final,
                ID,
                # elevation,
                stringybark,
                ribbonbark,
                LFMC,
                VPD,
                winddir:winddiff,
                category,
                aspect,
                firelines:water4,
                house.density) |> 
  filter(!is.na(LFMC))

g3 = rbind(g3.1, g3.2)

g3$ribbonbark[is.na(g3$ribbonbark)] = 0
g3$stringybark[is.na(g3$stringybark)] = 0
nrow(g3)
## 37,176

st_write(g3, "ch3_forGAMs_prefire180_final.gpkg", delete_dsn = T)
