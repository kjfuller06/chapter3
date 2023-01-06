library(sf)
library(raster)
library(tidyverse)
library(exactextractr)

# # wind direction quantiles ####
# setwd("/glade/scratch/kjfuller/data")
# # setwd("E:/chapter3/from Michael")
# wind = st_read("wind_direction.gpkg")
# # remove all directions when windspeed == 0; select just the stations for calculating distances
# wind = wind |>
#   filter(windspeed != 0)
# 
# setwd("/glade/scratch/kjfuller/data/chapter3")
# # setwd("E:/chapter3/for GAMS")
# g = st_read(paste0("ch3_forGAMs_prefire180_allvars.gpkg"))
# targetcrs = st_crs(g)
# g$poly_sD = as.POSIXct(g$lasttim, format = "%Y-%m-%d %H:%M")
# g$poly_eD = as.POSIXct(g$time, format = "%Y-%m-%d %H:%M")
# g = g |>
#   dplyr::select(ID,
#                 shot_number,
#                 poly_sD,
#                 poly_eD)
# g_buffer = st_buffer(g, dist = 100000) ## buffer of 100km to select stations
# g_buffer = st_transform(g_buffer, st_crs(wind))
# g = st_transform(g, st_crs(wind))
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
#     # winddir = aggregate(data = wind_temp, winddir ~ station, FUN = median)
#     # names(winddir)[2] = "winddir"
#     
#     windsp1 = aggregate(data = wind_temp, windspeed ~ station, FUN = function(x) median(x, na.rm = T))
#     names(windsp1)[2] = "windspeed"
# 
#     windsp2 = aggregate(data = wind_temp, windspeed ~ station, FUN = function(x) quantile(x, probs = 0.9, na.rm = T))
#     names(windsp2)[2] = "windspeed.9"
# 
#     windsp3 = aggregate(data = wind_temp, windspeed ~ station, FUN = function(x) quantile(x, probs = 0.1, na.rm = T))
#     names(windsp3)[2] = "windspeed.1"
# 
#     windgt1 = aggregate(data = wind_temp, windgust ~ station, FUN = function(x) median(x, na.rm = T))
#     names(windgt1)[2] = "windgust"
# 
#     windgt2 = aggregate(data = wind_temp, windgust ~ station, FUN = function(x) quantile(x, probs = 0.9, na.rm = T))
#     names(windgt2)[2] = "windgust.9"
# 
#     wind_temp = stat_temp |>
#       # inner_join(winddir) |> 
#       inner_join(windsp1) |>
#       inner_join(windsp2) |>
#       inner_join(windsp3) |>
#       inner_join(windgt1) |>
#       inner_join(windgt2)
#     # create distance-based weights and calculate distance-weighted mean and wind impact index
#     wind_temp$weight = 1 - (wind_temp$dist)/max(wind_temp$dist+1000)
#     # g_temp$winddir = weighted.mean(wind_temp$winddir, wind_temp$weight)
#     g_temp$windspeed = weighted.mean(wind_temp$windspeed, wind_temp$weight)
#     g_temp$windspeed.1 = weighted.mean(wind_temp$windspeed.1, wind_temp$weight)
#     g_temp$windspeed.9 = weighted.mean(wind_temp$windspeed.9, wind_temp$weight)
#     g_temp$windgust = weighted.mean(wind_temp$windgust, wind_temp$weight)
#     g_temp$windgust.9 = weighted.mean(wind_temp$windgust.9, wind_temp$weight)
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
# g_temp = bind_rows(l)
# g = left_join(g, g_temp)
# g = st_transform(g, crs = targetcrs)
# 
# setwd("/glade/scratch/kjfuller/data/chapter3")
# st_write(g, paste0("ch3_forGAMs_prefire180_wind.gpkg"), delete_dsn = T)
# 
# wind direction quantiles- redo ####
setwd("/glade/scratch/kjfuller/data/chapter3")
# setwd("E:/chapter3/for GAMS")
g = st_read(paste0("ch3_forGAMs_prefire180_allvars.gpkg"))
targetcrs = st_crs(g)
g$poly_sD = as.POSIXct(g$lasttim, format = "%Y-%m-%d %H:%M")
g$poly_eD = as.POSIXct(g$time, format = "%Y-%m-%d %H:%M")
g = g |>
  dplyr::select(ID,
                shot_number,
                poly_sD,
                poly_eD)

index = g |> 
  dplyr::select(poly_sD,
                poly_eD)
st_geometry(index) = NULL
index = unique(index)
index$index = c(1:nrow(index))
g = g |> 
  left_join(index)
# g = g[1:100,]

setwd("/glade/scratch/kjfuller/data")
# setwd("E:/chapter3/from Michael")
# wind = st_read("wind_direction.gpkg")
# # remove all directions when windspeed == 0; select just the stations for calculating distances
# wind = wind |>
#   filter(windspeed != 0) |> 
#   st_transform(crs = targetcrs)
# st_write(wind, "proj_wind_direction.gpkg", delete_dsn = T)
wind = st_read("proj_wind_direction.gpkg")

g_buffer = st_buffer(g, dist = 100000) ## buffer of radius 100km to select stations

stations = wind |>
  dplyr::select(station)
stations = stations[!duplicated(stations$station),]
st_geometry(wind) = NULL

counter <- 0
l <- list(NULL)
size <- 1
windfun = function(x){
  tryCatch({
    if( .GlobalEnv$counter == .GlobalEnv$size )
    {length(.GlobalEnv$l) <- .GlobalEnv$size <- .GlobalEnv$size * 2}
    
    # select a buffered shot and index all stations within 100km
    g_temp = g_buffer[g_buffer$index == unique(g_buffer$index)[x],]
    stat_temp = stations[g_temp,]
    
    # select the original shot and calculate distance to selected stations
    g_temp = g |>
      filter(shot_number %in% g_temp$shot_number)
    dist = as.data.frame(st_distance(g_temp, stat_temp))
    st_geometry(stat_temp) = NULL
    st_geometry(g_temp) = NULL
    stat_temp = cbind(stat_temp, t(dist))
    
    # if the polygon is the first, overwrite poly_sD as 24hrs before poly_eD (in minutes)
    if(min(g_temp$poly_sD) == max(g_temp$poly_eD)){
      g_temp$poly_sD = g_temp$poly_eD - 86400
    }
    
    # select wind data based on polygon timeframe
    wind_temp = wind |>
      filter(station %in% stat_temp$station) |>
      left_join(stat_temp)
    minT = wind_temp |>
      filter(DateTime <= min(g_temp$poly_sD))
    minT = max(minT$DateTime, na.rm = T)
    maxT = wind_temp |>
      filter(DateTime >= max(g_temp$poly_eD))
    maxT = min(maxT$DateTime, na.rm = T)
    wind_temp = wind_temp |>
      filter(DateTime >= minT) |>
      filter(DateTime <= maxT)
    
    # calculate the median wind speed within the polygon timeframe at each selected station, join to station data
    winddir = aggregate(data = wind_temp, winddir ~ station, FUN = function(x) median(x, na.rm = T))
    names(winddir)[2] = "winddir"
    
    windsp1 = aggregate(data = wind_temp, windspeed ~ station, FUN = function(x) median(x, na.rm = T))
    names(windsp1)[2] = "windspeed"

    windsp2 = aggregate(data = wind_temp, windspeed ~ station, FUN = function(x) quantile(x, probs = 0.9, na.rm = T))
    names(windsp2)[2] = "windspeed.9"

    windsp3 = aggregate(data = wind_temp, windspeed ~ station, FUN = function(x) quantile(x, probs = 0.1, na.rm = T))
    names(windsp3)[2] = "windspeed.1"

    windgt1 = aggregate(data = wind_temp, windgust ~ station, FUN = function(x) median(x, na.rm = T))
    names(windgt1)[2] = "windgust"

    windgt2 = aggregate(data = wind_temp, windgust ~ station, FUN = function(x) quantile(x, probs = 0.9, na.rm = T))
    names(windgt2)[2] = "windgust.9"
    
    wind_temp = stat_temp |>
      right_join(winddir) |> 
      full_join(windsp1) |>
      full_join(windsp2) |>
      full_join(windsp3) |>
      full_join(windgt1) |>
      full_join(windgt2)
    # create distance-based weights and calculate distance-weighted mean and wind impact index
    wind_temp[,c(2:(ncol(wind_temp) - 6))] = as.data.frame(lapply(wind_temp |> 
                                                                    dplyr::select(c(2:(ncol(wind_temp) - 6))), 
                                                                  function(x) {1 - (x/100000)}))
    g_temp$winddir = as.numeric(lapply(wind_temp |> 
                              dplyr::select(c(2:(ncol(wind_temp) - 6))), 
                            function(x) {weighted.mean(wind_temp[,"winddir"], x, na.rm = T)}))
    g_temp$windspeed = as.numeric(lapply(wind_temp |> 
                                dplyr::select(c(2:(ncol(wind_temp) - 6))), 
                              function(x) {weighted.mean(wind_temp[,"windspeed"], x, na.rm = T)}))
    g_temp$windspeed.1 = as.numeric(lapply(wind_temp |> 
                                  dplyr::select(c(2:(ncol(wind_temp) - 6))), 
                                function(x) {weighted.mean(wind_temp[,"windspeed.1"], x, na.rm = T)}))
    g_temp$windspeed.9 = as.numeric(lapply(wind_temp |> 
                                  dplyr::select(c(2:(ncol(wind_temp) - 6))), 
                                function(x) {weighted.mean(wind_temp[,"windspeed.9"], x, na.rm = T)}))
    g_temp$windgust = as.numeric(lapply(wind_temp |> 
                               dplyr::select(c(2:(ncol(wind_temp) - 6))), 
                             function(x) {weighted.mean(wind_temp[,"windgust"], x, na.rm = T)}))
    g_temp$windgust.9 = as.numeric(lapply(wind_temp |> 
                                 dplyr::select(c(2:(ncol(wind_temp) - 6))), 
                               function(x) {weighted.mean(wind_temp[,"windgust.9"], x, na.rm = T)}))
    # g_temp$winddir = weighted.mean(wind_temp$winddir, wind_temp$weight)
    # g_temp$windspeed = weighted.mean(wind_temp$windspeed, wind_temp$weight)
    # g_temp$windspeed.1 = weighted.mean(wind_temp$windspeed.1, wind_temp$weight)
    # g_temp$windspeed.9 = weighted.mean(wind_temp$windspeed.9, wind_temp$weight)
    # g_temp$windgust = weighted.mean(wind_temp$windgust, wind_temp$weight)
    # g_temp$windgust.9 = weighted.mean(wind_temp$windgust.9, wind_temp$weight)
    
    .GlobalEnv$counter <- .GlobalEnv$counter + 1
    .GlobalEnv$l[[.GlobalEnv$counter]] <- g_temp
    
  }, error = function(e){print(x); cat("ERROR :", conditionMessage(e), "\n")})
}
for(i in c(1:length(unique(g_buffer$index)))){
  windfun(i)
  print(i)
}
g_temp = bind_rows(l)
# setwd("/glade/scratch/kjfuller/data/chapter3")
# write.csv(g_temp, "ch3_forGAMs_prefire180_wind2.csv", row.names = F)

g = left_join(g, g_temp)
st_write(g, paste0("ch3_forGAMs_prefire180_wind2.gpkg"), delete_dsn = T)
