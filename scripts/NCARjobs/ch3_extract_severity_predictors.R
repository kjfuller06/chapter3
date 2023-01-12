library(sf)
library(raster)
library(tidyverse)
library(exactextractr)

# # load files ####
# setwd("/glade/scratch/kjfuller/data")
# houses = read.csv("housing_density.csv") |>
#   dplyr::select(ID, house.density)
# 
# setwd("/glade/scratch/kjfuller/data")
# # # setwd("E:/chapter3/from Michael")
# wind = st_read("wind_direction.gpkg")
# # remove all directions when windspeed == 0; select just the stations for calculating distances
# wind = wind |>
#   filter(windspeed != 0)
# 
# types = read.csv("FESM_firetypes.csv") |>
#   dplyr::select(fire_id, category)
# types$fire_id = as.factor(types$fire_id)
# 
# setwd("E:/chapter3")
# types = read.csv("FESM_firetypes.csv")
# types$fire_id = as.factor(types$fire_id)
# 
# setwd("/glade/scratch/kjfuller/data/LFMC")
# # setwd("E:/chapter3/from Rachael/R project/LFMC/LFMC/GEE_LFMC/LFMC_rasters/GEE_v20May2020_RevisedMask/")
# lfmc = list.files()
# lfmc = data.frame(files = lfmc)
# lfmc$date = as.POSIXct(substr(lfmc$files, 6, 15), format = "%Y_%m_%d")
# ## using as.Date led to problems for this extraction but the severity data extraction did fine- double-checked
# 
# setwd("/glade/scratch/kjfuller/data/VPD")
# # setwd("E:/chapter3/from Rachael/VPD")
# vpd = list.files()
# vpd = data.frame(files = vpd)
# vpd$date = as.POSIXct(substr(vpd$files, 5, 12), format = "%Y%m%d")
# 
# # done #####
# # setwd("E:/chapter3/GEDI_FESM")
# extractfun = function(x){
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
#     vpd_temp = vpd |>
#       filter(as.numeric(difftime(min(g_temp$poly_sD), date)) <= 0)
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
#   
# # merge housing density, distance to breaks and fire types ####
# # setwd("E:/chapter3/for GAMs")
# setwd("/glade/scratch/kjfuller/data/chapter3")
# g = st_read(paste0("ch3_forGAMs_prefire", x, "_allvars.gpkg"))
# targetcrs = st_crs(g)
# g$fire_id = as.factor(g$fire_id)
# g = g |>
#   left_join(houses) |>
#   left_join(types)
# 
# # aspect ####
# setwd("/glade/scratch/kjfuller/data")
# r = raster("proj_dem_aspect_30m.tif")
# 
# g = st_transform(g, crs = st_crs(r))
# g_buffer = st_buffer(g, dist = 12.5)
# 
# g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
# names(g_temp)[names(g_temp) == "mode"] = "aspect"
# g = left_join(g, g_temp)
# 
# # distance to firelines ####
# r = raster("distancetofirelines_parallel.tif")
# 
# g = st_transform(g, crs = st_crs(r))
# g_buffer = st_buffer(g, dist = 12.5)
# 
# g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
# names(g_temp)[names(g_temp) == "mode"] = "firelines"
# g = left_join(g, g_temp)
# 
# # distance to roads ####
# r = raster("distancetoroads.tif")
# 
# g = st_transform(g, crs = st_crs(r))
# g_buffer = st_buffer(g, dist = 12.5)
# 
# g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
# names(g_temp)[names(g_temp) == "mode"] = "roads"
# g = left_join(g, g_temp)
# 
# # distance to waterways ####
# r = raster("distancetowater_relevance2.tif")
# 
# g = st_transform(g, crs = st_crs(r))
# g_buffer = st_buffer(g, dist = 12.5)
# 
# g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
# names(g_temp)[names(g_temp) == "mode"] = "water2"
# g = left_join(g, g_temp)
# 
# r = raster("distancetowater_relevance3.tif")
# 
# g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
# names(g_temp)[names(g_temp) == "mode"] = "water3"
# g = left_join(g, g_temp)
# 
# r = raster("distancetowater_relevance4.tif")
# 
# g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
# names(g_temp)[names(g_temp) == "mode"] = "water4"
# g = left_join(g, g_temp)
# 
# # wind direction ####
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
# l = list()
# for(i in c(1:nrow(g))){
#   # select a buffered shot and index all stations within 100km
#   g_temp = g_buffer[i,]
#   stat_temp = stations[g_temp,]
#   
#   # select the original shot and calculate distance to selected stations
#   g_temp = g |>
#     filter(shot_number %in% g_temp$shot_number)
#   stat_temp$dist = as.numeric(st_distance(g_temp, stat_temp))
#   st_geometry(stat_temp) = NULL
#   st_geometry(g_temp) = NULL
#   
#   # if the polygon is the first, overwrite poly_sD as 24hrs before poly_eD (in minutes)
#   if(min(g_temp$poly_sD) == max(g_temp$poly_eD)){
#     g_temp$poly_sD = g_temp$poly_eD - 86400
#   }
#   
#   # select wind data based on polygon timeframe
#   wind_temp = wind |>
#     filter(station %in% stat_temp$station) |>
#     left_join(stat_temp)
#   minT = wind_temp |>
#     filter(DateTime <= min(g_temp$poly_sD))
#   minT = max(minT$DateTime, na.rm = T)
#   maxT = wind_temp |>
#     filter(DateTime >= max(g_temp$poly_eD))
#   maxT = min(maxT$DateTime, na.rm = T)
#   wind_temp = wind_temp |>
#     filter(DateTime >= minT) |>
#     filter(DateTime <= maxT)
#   
#   # calculate the median wind speed within the polygon timeframe at each selected station, join to station data
#   winddir = aggregate(data = wind_temp, winddir ~ station, FUN = median)
#   names(winddir)[2] = "winddir"
#   windstats = nrow(winddir)
#   windsp = aggregate(data = wind_temp, windspeed ~ station, FUN = median)
#   names(windsp)[2] = "windspeed"
#   windgt = aggregate(data = wind_temp, windgust ~ station, FUN = median)
#   names(windgt)[2] = "windgust"
#   
#   wind_temp = stat_temp |>
#     inner_join(winddir) |>
#     inner_join(windsp) |>
#     inner_join(windgt)
#   # write number of stations used for calculation
#   wind_temp$windstats = windstats
#   # create distance-based weights and calculate distance-weighted mean and wind impact index
#   wind_temp$weight = 1 - (wind_temp$dist)/max(wind_temp$dist)
#   g_temp$winddir = weighted.mean(wind_temp$winddir, wind_temp$weight)
#   g_temp$windspeed = weighted.mean(wind_temp$windspeed, wind_temp$weight)
#   g_temp$windgust = weighted.mean(wind_temp$windgust, wind_temp$weight)
#   
#   g_temp$winddiff = g_temp$aspect - g_temp$winddir
#   g_temp$winddiff = cos(g_temp$winddiff * pi / 180)
#   l[[i]] = g_temp
#   print(i)
# }
# g_temp = bind_rows(l)
# g = full_join(g, g_temp)
# 
# setwd("/glade/scratch/kjfuller/data/chapter3")
# st_write(g, paste0("ch3_forGAMs_prefire", x, "_allvars2.gpkg"), delete_dsn = T)
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
# types = read.csv("FESM_firetypes.csv") |>
#   dplyr::select(fire_id, category)
# types$fire_id = as.factor(types$fire_id)
# 
# # merge housing density, distance to breaks and fire types
# # setwd("E:/chapter3/for GAMs")
# setwd("/glade/scratch/kjfuller/data/chapter3")
# g = st_read(paste0("ch3_forGAMs_prefire180_allvars.gpkg"))
# targetcrs = st_crs(g)
# g$fire_id = as.factor(g$fire_id)
# g = g |>
#   left_join(types)
# 
# # aspect
# setwd("/glade/scratch/kjfuller/data")
# r = raster("proj_dem_aspect_30m.tif")
# 
# g = st_transform(g, crs = st_crs(r))
# g_buffer = st_buffer(g, dist = 12.5)
# 
# g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
# names(g_temp)[names(g_temp) == "mode"] = "aspect"
# g = left_join(g, g_temp)
# 
# # distance to firelines
# r = raster("distancetofirelines_parallel.tif")
# 
# g = st_transform(g, crs = st_crs(r))
# g_buffer = st_buffer(g, dist = 12.5)
# 
# g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
# names(g_temp)[names(g_temp) == "mode"] = "firelines"
# g = left_join(g, g_temp)
# 
# # distance to roads
# r = raster("distancetoroads.tif")
# 
# g = st_transform(g, crs = st_crs(r))
# g_buffer = st_buffer(g, dist = 12.5)
# 
# g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
# names(g_temp)[names(g_temp) == "mode"] = "roads"
# g = left_join(g, g_temp)
# 
# # distance to waterways
# r = raster("distancetowater_relevance2.tif")
# 
# g = st_transform(g, crs = st_crs(r))
# g_buffer = st_buffer(g, dist = 12.5)
# 
# g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
# names(g_temp)[names(g_temp) == "mode"] = "water2"
# g = left_join(g, g_temp)
# 
# r = raster("distancetowater_relevance3.tif")
# 
# g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
# names(g_temp)[names(g_temp) == "mode"] = "water3"
# g = left_join(g, g_temp)
# 
# r = raster("distancetowater_relevance4.tif")
# 
# g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
# names(g_temp)[names(g_temp) == "mode"] = "water4"
# g = left_join(g, g_temp)
# 
# setwd("/glade/scratch/kjfuller/data/chapter3")
# st_write(g, paste0("ch3_forGAMs_prefire180_dynamic.gpkg"), delete_dsn = T)
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

setwd("E:/chapter3")
types = read.csv("FESM_firetypes.csv")
types$category[types$firetype == "lightning" | types$firetype == "natural"] = "wildfire"
types$category[types$firetype != "lightning" & types$firetype != "natural"] = "non-wildfire"
types$fire_id = as.factor(types$fire_id)

# prefire 180
setwd("E:/chapter3/for GAMs")
## outdated files:
# g1 = st_read("ch3_forGAMs_prefire180_static.gpkg") ## 62 vars, 52,462 rows
# ## shot data, static vars
# g2 = st_read("ch3_forGAMs_prefire180_allvars.gpkg") ## 66 vars, 52,462 rows
# ## same + LFMC, VPD
# g4 = st_read("ch3_forGAMs_prefire180_allvars2.gpkg") ## 78 vars, 29,334 rows
# ## same vars, with only unique shots + wind data

## below files contain all relevant vars:
g3 = st_read("ch3_forGAMs_prefire180_dynamic.gpkg") ## 74 vars, 52,462 rows
## same + house density, category, aspect, breaks (do not use LFMC or house density; need to update "category")
g3 = g3 |>
  dplyr::select(-house.density, -LFMC, -LFMC_sD, -category) |>
  left_join(housing) |> 
  left_join(types) |> 
  filter(!is.na(category))
summary(g3)

g5 = st_read("ch3_forGAMs_prefire180_LFMC.gpkg") ## 7 vars and 52,462 rows
## shot and fire IDs, dates, LFMC
st_geometry(g5) = NULL
g5 = g5 |> 
  dplyr::select(shot_number,
                ID,
                LFMC,
                LFMC_sD) |> 
  filter(!is.na(LFMC))
summary(g5)

g6 = st_read("ch3_forGAMS_prefire180_wind2.gpkg") ## 12 vars and 52,462 rows
## shot and fire IDs, dates, wind data
st_geometry(g6) = NULL
g6 = g6 |> 
  dplyr::select(shot_number,
                ID,
                winddir,
                wind.stdev,
                windspeed,
                windspeed.1,
                windspeed.9,
                windgust,
                windgust.9) |> 
  filter(!is.na(winddir)) |> 
  filter(!is.na(windgust)) |> 
  filter(!is.na(wind.stdev))
summary(g6)
any(g6$windspeed < g6$windspeed.1)
## FALSE
any(g6$windspeed > g6$windspeed.9)
## FALSE
any(g6$windspeed > g6$windgust)
## TRUE
plot(g6$windgust ~ g6$windspeed)
abline(0, 1)
g6$windgust[g6$windspeed > g6$windgust] = g6$windspeed[g6$windspeed > g6$windgust]
plot(g6$windgust ~ g6$windspeed)
abline(0, 1)
any(g6$windspeed > g6$windgust)
## FALSE
any(g6$windspeed > g6$windgust.9)
## FALSE
any(g6$winddir < 0 | g6$winddir > 359)
## FALSE

all = g3 |>
  left_join(g5) |> 
  left_join(g6) |> 
  filter(!is.na(windgust)) |> 
  filter(!is.na(wind.stdev)) |> 
  filter(!is.na(LFMC)) |> 
  filter(!is.na(stringybark)) |> 
  filter(!is.na(ribbonbark)) |> 
  filter(fire_reg != 7 & fire_reg != 9)
summary(all)

all$winddiff.iso = cos((all$aspect - all$maxwd) * pi / 180)
all$winddiff.bom = cos((all$aspect - all$winddir) * pi / 180)

all = all |>
  dplyr::select(severity,
                shot_number,
                veg,
                fire_reg,
                rh98:over_cover,
                maxtemp:maxwd,
                ffdi_final,
                slope,
                # elevation,
                stringybark,
                ribbonbark,
                LFMC,
                VPD,
                winddiff.iso,
                winddiff.bom,
                winddir,
                windspeed,
                windspeed.1,
                windspeed.9,
                windgust,
                windgust.9,
                category,
                firelines:water4,
                house.density)

all$ffdi_cat = NA
all$ffdi_cat[all$ffdi_final <= 12] = "one"
all$ffdi_cat[all$ffdi_final > 12 & all$ffdi_final <= 25] = "two"
all$ffdi_cat[all$ffdi_final > 25 & all$ffdi_final <= 49] = "three"
all$ffdi_cat[all$ffdi_final > 49 & all$ffdi_final <= 74] = "four"
all$ffdi_cat[all$ffdi_final > 74 & all$ffdi_final <= 99] = "four"
all$ffdi_cat[all$ffdi_final > 99] = "four"
all$ffdi_cat = factor(all$ffdi_cat, levels = c("one",
                                                 "two",
                                                 "three",
                                                 "four"))
summary(all)
nrow(all)
## 41,769
st_write(all, "ch3_forGAMs_prefire180_final.gpkg", delete_dsn = T)

# prefire 7
g1 = st_read("ch3_forGAMs_prefire7_static.gpkg")

g1 = all |> 
  filter(shot_number %in% g1$shot_number)

nrow(g1)
## 21
st_write(g1, "ch3_forGAMs_prefire7_final.gpkg", delete_dsn = T)

# prefire 14
g1 = st_read("ch3_forGAMs_prefire14_static.gpkg")

g1 = all |> 
  filter(shot_number %in% g1$shot_number)

nrow(g1)
## 405
st_write(g1, "ch3_forGAMs_prefire14_final.gpkg", delete_dsn = T)

# prefire 30
g1 = st_read("ch3_forGAMs_prefire30_static.gpkg")

g1 = all |> 
  filter(shot_number %in% g1$shot_number)

nrow(g1)
## 3,422
st_write(g1, "ch3_forGAMs_prefire30_final.gpkg", delete_dsn = T)

# prefire 60
g1 = st_read("ch3_forGAMs_prefire60_static.gpkg")

g1 = all |> 
  filter(shot_number %in% g1$shot_number)

nrow(g1)
## 11,232
st_write(g1, "ch3_forGAMs_prefire60_final.gpkg", delete_dsn = T)

# prefire 90
g1 = st_read("ch3_forGAMs_prefire90_static.gpkg")

g1 = all |> 
  filter(shot_number %in% g1$shot_number)

nrow(g1)
## 18,383
st_write(g1, "ch3_forGAMs_prefire90_final.gpkg", delete_dsn = T)