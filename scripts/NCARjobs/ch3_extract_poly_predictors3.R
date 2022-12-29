library(sf)
library(raster)
library(tidyverse)
library(exactextractr)
library(rgeos)

setwd("/glade/scratch/kjfuller/data")
# setwd("E:/chapter3/from Michael")
wind = st_read("wind_direction.gpkg")
# remove all directions when windspeed == 0; select just the stations for calculating distances
wind = wind |> 
  filter(windspeed != 0)

# load isochrons ####
setwd("/glade/scratch/kjfuller/data/chapter3")
# setwd("E:/chapter3/for GAMs")
g = st_read("ch3_forGAMs_poly_prefire180_structure.gpkg")
targetcrs = st_crs(g)
g = g |> 
  dplyr::select(ID,
                poly_sD,
                poly_eD)
g_buffer = st_buffer(g, dist = 100000)
g_buffer = st_transform(g_buffer, st_crs(wind))

stations = wind |> 
  dplyr::select(station)
stations = stations[!duplicated(stations$station),]
st_geometry(wind) = NULL

counter <- 0
l <- list(NULL)
size <- 1
windfun = function(x){
  if( .GlobalEnv$counter == .GlobalEnv$size )
  {length(.GlobalEnv$l) <- .GlobalEnv$size <- .GlobalEnv$size * 2}
  
  tryCatch({
    # select a buffered polygon and index all stations within 100km
    g_temp = g_buffer[x,]
    stat_temp = stations[g_temp,]
    
    # select the original polygon and calculate distance from centroid to selected stations
    g_temp = g |> 
      filter(ID %in% g_temp$ID)
    g_center = st_centroid(g_temp)
    g_center = st_transform(g_center, st_crs(g_buffer))
    stat_temp$dist = as.numeric(st_distance(g_center, stat_temp))
    st_geometry(stat_temp) = NULL
    st_geometry(g_temp) = NULL
    
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
    winddir = aggregate(data = wind_temp, winddir ~ station, FUN = median)
    names(winddir)[2] = "winddir"
    windsp = aggregate(data = wind_temp, windspeed ~ station, FUN = median)
    names(windsp)[2] = "windspeed"
    windgt = aggregate(data = wind_temp, windgust ~ station, FUN = median)
    names(windgt)[2] = "windgust"
    
    wind_var = aggregate(data = wind_temp, winddir ~ station, FUN = sd)
    names(wind_var)[2] = "wind.stdev"
    wind_temp = stat_temp |> 
      inner_join(winddir) |> 
      inner_join(wind_var) |> 
      inner_join(windsp) |> 
      inner_join(windgt)
    # write number of stations used for calculation
    g_temp$windstats = nrow(winddir)
    # create distance-based weights and calculate distance-weighted mean and wind impact index
    wind_temp$weight = 1 - (wind_temp$dist)/max(wind_temp$dist+1000)
    g_temp$winddir = weighted.mean(wind_temp$winddir, wind_temp$weight)
    g_temp$wind.stdev = weighted.mean(wind_temp$wind.stdev, wind_temp$weight)
    g_temp$windspeed = weighted.mean(wind_temp$windspeed, wind_temp$weight)
    g_temp$windgust = weighted.mean(wind_temp$windgust, wind_temp$weight)
    
    # g_temp$winddiff = g_temp$aspect - g_temp$winddir ## aspect not extracted yet
    # g_temp$winddiff = cos(g_temp$winddiff * pi / 180)
    
    # l[[x]] = g_temp
    
    .GlobalEnv$counter <- .GlobalEnv$counter + 1
    .GlobalEnv$l[[.GlobalEnv$counter]] <- g_temp
    
  }, error = function(e){print(x); cat("ERROR :", conditionMessage(e), "\n")})
}
for(i in c(1:nrow(g))){
  windfun(i)
  print(i)
}
print("binding rows")
g_temp = bind_rows(l)
print("joining to original isochron data")
g = left_join(g, g_temp)

# write to disk ####
setwd("/glade/scratch/kjfuller/data/chapter3")
st_write(g, paste0("ch3_forGAMs_poly_prefire180_wind.gpkg"), delete_dsn = T)

## still need to extract: fire type categories (FESM directly) -> not all isochrons align with FESM fires
## aspect, wind direction done, fire regime types (in order to restrict less representative veg types), number of dwellings, distance to roads and distance to water (take the min of both) done