library(sf)
library(raster)
library(tidyverse)
library(exactextractr)
library(rgeos)

setwd("/glade/scratch/kjfuller/data")
access = st_read("access_lines.gpkg")
# setwd("E:/chapter3/waterways")
water = st_read("water.gpkg")
# setwd("E:/chapter3/from Michael")
wind = st_read("wind_direction.gpkg")

setwd("/glade/scratch/kjfuller/data/LFMC")
# setwd("E:/chapter3/from Rachael/R project/LFMC/LFMC/GEE_LFMC/LFMC_rasters/GEE_v20May2020_RevisedMask/")
lfmc = list.files()
lfmc = data.frame(files = lfmc)
lfmc$date = as.Date(substr(lfmc$files, 6, 15), format = "%Y_%m_%d")

setwd("/glade/scratch/kjfuller/data/VPD")
# setwd("E:/chapter3/from Rachael/VPD")
vpd = list.files()
vpd = data.frame(files = vpd)
vpd$date = as.Date(substr(vpd$files, 5, 12), format = "%Y%m%d")

setwd("/glade/scratch/kjfuller/data")
fire_reg = read.csv("fire_regimes.csv") |> 
  dplyr::select(fueltype,
                fire_reg)

setwd("/glade/scratch/kjfuller/data/chapter3")
extractfun = function(x){
  # setwd("E:/chapter3/GEDI_FESM")
  gedi = st_read(paste0("ch3_isochrons_prefire", x, ".gpkg"))
  targetcrs = st_crs(gedi)
  st_geometry(gedi) = NULL
  gedi = gedi |> 
    dplyr::select(shot_number,
                  ID,
                  rh98,
                  cover_z_1,
                  over_cover,
                  fhd_normal)
  
  setwd("/glade/scratch/kjfuller/data")
  # setwd("E:/chapter3/isochrons/Progall_ffdi_v3")
  g = st_read("progall_ffdi_v3.shp")
  g$ID = c(1:nrow(g))
  g$poly_sD = as.POSIXct(g$lasttim, format = "%Y-%m-%d %H:%M")
  g$poly_eD = as.POSIXct(g$time, format = "%Y-%m-%d %H:%M")
  g$progtime = difftime(g$poly_eD, g$poly_sD, units = "hours")
  g$progtime = as.numeric(g$progtime)
  g = g |> 
    filter(progtime > 0) |> 
    filter(progtime <= 24)
  g$prog = g$area/g$progtime
  
  g_agg = g |> 
    right_join(gedi)
  # calculate the median of continuous variables
  g_agg1 = aggregate(data = g_agg, rh98 ~ ID, FUN = median)
  g_agg2 = aggregate(data = g_agg, cover_z_1 ~ ID, FUN = median)
  g_agg3 = aggregate(data = g_agg, over_cover ~ ID, FUN = median)
  g_agg4 = aggregate(data = g_agg, fhd_normal ~ ID, FUN = median)
  
  g = g |> 
    right_join(g_agg1) |> 
    right_join(g_agg2) |> 
    right_join(g_agg3) |> 
    right_join(g_agg4)
  
  # load rasters
  # setwd("D:/chapter1/other_data/Final/terrain variables")
  
  # elevation
  r = raster("proj_dem_s.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g$elevation = raster::extract(r, g, method = 'simple', fun = sd)
  
  g_na = g |> 
    filter(is.na(elevation))
  elev_na = raster::extract(r, g_na, method = 'simple', exact = T)
  g_na$elevation = unlist(lapply(elev_na, sd))
  
  g = g |> 
    filter(!is.na(elevation))
  g = rbind(g, g_na)
  
  # fire regime
  r = raster("fuels_30m.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g$fueltype = raster::extract(r, g, method = 'simple', fun = median)
  g = g |> 
    left_join(fire_reg)
  
  # slope
  r = raster("proj_dem_slope_30m.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g$slope = raster::extract(r, g, method = 'simple', fun = median)
  
  # aspect
  r = raster("proj_dem_aspect_30m.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g$aspect = raster::extract(r, g, method = 'simple', fun = median)
  
  # northness
  r = raster("proj_dem_northness_30m.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g$northness = raster::extract(r, g, method = 'simple', fun = median)
  
  # eastness
  r = raster("proj_dem_eastness_30m.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g$eastness = raster::extract(r, g, method = 'simple', fun = median)
  
  # bark types
  r = raster("NSW_stringybark_distribution.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g$stringybark = raster::extract(r, g, method = 'simple', fun = median)
  
  r = raster("NSW_ribboning_distribution.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g$ribbonbark = raster::extract(r, g, method = 'simple', fun = median)
  
  setwd("/glade/scratch/kjfuller/data/chapter3")
  g = st_transform(g, crs = targetcrs)
  st_write(g, paste0("ch3_forGAMs_poly_prefire", x, "_static.gpkg"), delete_dsn = T)
  # g = st_read(paste0("ch3_forGAMs_poly_prefire", x, "_static.gpkg"))
  
  # LFMC
  setwd("/glade/scratch/kjfuller/data/LFMC")
  # setwd("E:/chapter3/from Rachael/R project/LFMC/LFMC/GEE_LFMC/LFMC_rasters/GEE_v20May2020_RevisedMask/")
  r = raster(lfmc$files[1])
  g = st_transform(g, crs = st_crs(r))
  ids = unique(g$ID)
  l = list()
  for(i in c(1:length(ids))){
    g_temp = g |>
      filter(ID == ids[i])
    
    ## select LFMC values that were calculated for dates before the fire started
    lfmc_temp = lfmc |>
      filter(as.numeric(difftime(min(g_temp$poly_sD), date)) > 0)
    ## select the last possible pre-fire LFMC value
    lfmc_temp = lfmc_temp |>
      filter(date == max(lfmc_temp$date))
    
    r = raster(lfmc_temp$files[1])
    
    g_temp$LFMC = raster::extract(r, g_temp, method = 'simple', fun = median)
    g_temp$LFMC_sD = lfmc_temp$date[1]
    
    if(any(is.na(g_temp$LFMC))){
      g_temp2 = g_temp |>
        filter(is.na(LFMC))
      if(g_temp2$LFMC_sD != min(lfmc$date)){
        lfmc_temp = lfmc |>
          filter(date < g_temp2$LFMC_sD[1])
        lfmc_temp = lfmc_temp |>
          filter(date == max(lfmc_temp$date))
        
        r = raster(lfmc_temp$files[1])
        
        g_temp2$LFMC = raster::extract(r, g_temp2, method = 'simple', fun = median)
        g_temp2$LFMC_sD = lfmc_temp$date[1]
        
        g_temp = g_temp |>
          filter(!is.na(LFMC)) |>
          rbind(g_temp2)
      }
    }
    l[[i]] = g_temp
    print(paste0("LFMC ", i))
  }
  g_temp = bind_rows(l)
  st_geometry(g_temp) = NULL
  g = full_join(g, g_temp)
  
  # VPD
  setwd("/glade/scratch/kjfuller/data/VPD")
  # setwd("E:/chapter3/from Rachael/VPD")
  r = raster(vpd$files[1])
  g = st_transform(g, crs = st_crs(4326))
  ids = unique(g$ID)
  l = list()
  for(i in c(1:length(ids))){
    g_temp = g |>
      filter(ID == ids[i])
    
    ## select VPD values that were calculated for dates after the fire started; for the fire polygon for each fire, extract values for the previous 24 hours
    vpd_temp = vpd |>
      filter(as.numeric(difftime(min(g_temp$poly_sD), date)) <= 0)
    vpd_temp = vpd_temp |>
      filter(as.numeric(difftime(min(g_temp$poly_eD), date)) >= 0)
    
    r = raster::stack(vpd_temp$files)
    
    g_temp$VPD = median(raster::extract(r, g_temp, method = 'simple', fun = median))
    g_temp$VPD_sD = vpd_temp$date[1]
    
    l[[i]] = g_temp
    print(paste0("VPD ", i))
  }
  g_temp = bind_rows(l)
  st_geometry(g_temp) = NULL
  g = full_join(g, g_temp)
  
  # wind direction
  g = st_transform(g, crs = targetcrs)
  g_buffer = st_buffer(g, dist = 100000)
  g_buffer = st_transform(g_buffer, st_crs(wind))
  
  # select just the stations for calculating distances
  stations = wind |> 
    dplyr::select(station)
  stations = stations[!duplicated(stations$station),]
  st_geometry(wind) = NULL
  
  l = list()
  for(i in c(1:nrow(g))){
    # select a buffered polygon and index all stations within 100km
    g_temp = g_buffer[i,]
    stat_temp = stations[g_temp,]
    
    # select the original polygon and calculate distance from centroid to selected stations
    g_temp = g |> 
      filter(ID %in% g_temp$ID)
    g_center = st_centroid(g_temp)
    g_center = st_transform(g_center, st_crs(g_buffer))
    stat_temp$dist = as.numeric(st_distance(g_center, stat_temp))
    st_geometry(stat_temp) = NULL
    st_geometry(g_temp) = NULL
    
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
    wind_med = aggregate(data = wind_temp, winddir ~ station, FUN = median)
    names(wind_med)[2] = "wind.median"
    wind_var = aggregate(data = wind_temp, winddir ~ station, FUN = sd)
    names(wind_var)[2] = "wind.stdev"
    wind_temp = stat_temp |> 
      inner_join(wind_med) |> 
      inner_join(wind_var)
    # write number of stations used for calculation
    wind_temp$windstats = nrow(wind_med)
    # create distance-based weights and calculate distance-weighted mean and wind impact index
    wind_temp$weight = 1 - (wind_temp$dist)/max(wind_temp$dist+1000)
    g_temp$windmean = weighted.mean(wind_temp$wind.median, wind_temp$weight)
    g_temp$winddiff = g_temp$aspect - g_temp$windmean
    g_temp$winddiff = cos(g_temp$winddiff * pi / 180)
    # select relevant variables for passing to final df
    g_temp = g_temp |> 
      dplyr::select(shot_number,
                    winddir,
                    winddiff,
                    windstats)
    
    g_temp = st_transform(g_temp, crs = targetcrs)
    
    l[[i]] = g_temp
  }
  g_temp = bind_rows(l)
  g = full_join(g, g_temp)
  
  # distance to water
  extent(g)
  r_temp = raster(ext = extent(g), res = 30, crs = crs(g))
  dd = gDistance(water, as(r_temp,"SpatialPoints"), byid=TRUE) ## need to trouble-shoot, need to subset by date created
  
  setwd("/glade/scratch/kjfuller/data/chapter3")
  st_write(g, paste0("ch3_forGAMs_prefire", x, "_poly_allvars.gpkg"), delete_dsn = T)
}

extractfun(7)
extractfun(14)
extractfun(30)
extractfun(60)
extractfun(90)
extractfun(180)

## still need to extract: fire type categories (FESM directly), distance to roads (or number of dwellings), distance to water 
## aspect, wind direction done, fire regime types (in order to restrict less representative veg types) done