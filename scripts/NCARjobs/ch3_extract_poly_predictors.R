library(sf)
library(raster)
library(tidyverse)
library(exactextractr)
library(rgeos)

setwd("/glade/scratch/kjfuller/data")
# setwd("E:/chapter3/from Michael")
wind = st_read("wind_direction.gpkg")
# setwd("E:/chapter3/dwellings")
houses = read.csv("housing_density.csv") |> 
  dplyr::select(ID, house.density)

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
# setwd("D:/chapter1/bark-type-SDM/data")
fire_reg = read.csv("fire_regimes.csv") |> 
  dplyr::select(fueltype,
                fire_reg)

setwd("/glade/scratch/kjfuller/data/chapter3")
extractfun = function(x){
  tryCatch({
  # load forest structure data ####
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
  
  # load isochrons ####
  setwd("/glade/scratch/kjfuller/data")
  # setwd("E:/chapter3/isochrons")
  g = st_read("isochrons_8days.gpkg")
  
  # merge housing density, distance to breaks, and fire types ####
  g = g |> 
    left_join(houses) 
  
  # calculate and merge structural data ####
  g_agg = g |> 
    inner_join(gedi)
  # calculate the median of continuous variables
  g_agg1 = aggregate(data = g_agg, rh98 ~ ID, FUN = median)
  g_agg2 = aggregate(data = g_agg, cover_z_1 ~ ID, FUN = median)
  g_agg3 = aggregate(data = g_agg, over_cover ~ ID, FUN = median)
  g_agg4 = aggregate(data = g_agg, fhd_normal ~ ID, FUN = median)
  
  print("joining all median values of GEDI structure to isochron data")
  g = g |> 
    right_join(g_agg1) |> 
    right_join(g_agg2) |> 
    right_join(g_agg3) |> 
    right_join(g_agg4)
  
  # load rasters
  # setwd("D:/chapter1/other_data/Final/terrain variables")
  
  # elevation ####
  print("reading elevation file")
  r = raster("proj_dem_s.tif")
  
  print("converting to elevation crs")
  g = st_transform(g, crs = st_crs(r))
  print("extracting elevation")
  g$elevation = raster::extract(r, g, method = 'simple', fun = sd)
  
  print("filtering to exclude all NA values")
  g_na = g |> 
    filter(is.na(elevation))
  if(nrow(g_na) > 0){
    print("some NA's exist: extracting elevation at NA locations again with another method")
    elev_na = raster::extract(r, g_na, method = 'simple', exact = T)
    g_na$elevation = unlist(lapply(elev_na, sd))
    
    g = g |> 
      filter(!is.na(elevation))
    g = rbind(g, g_na)
  }
  
  # fire regimes ####
  r = raster("fuels_30m.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g$fueltype = raster::extract(r, g, method = 'simple', fun = median)
  g = g |> 
    left_join(fire_reg)
  
  # slope ####
  r = raster("proj_dem_slope_30m.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g$slope = raster::extract(r, g, method = 'simple', fun = median)
  
  # aspect ####
  r = raster("proj_dem_aspect_30m.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g$aspect = raster::extract(r, g, method = 'simple', fun = median)
  
  # distance to firelines ####
  r = raster("distancetofirelines_parallel.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g$firelines = raster::extract(r, g, method = 'simple', fun = median)
  
  # distance to roads ####
  r = raster("distancetoroads.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g$roads = raster::extract(r, g, method = 'simple', fun = median)
  
  # distance to water ####
  r = raster("distancetowater_relevance2.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g$water2 = raster::extract(r, g, method = 'simple', fun = median)
  
  r = raster("distancetowater_relevance3.tif")
  g$water3 = raster::extract(r, g, method = 'simple', fun = median)
  
  r = raster("distancetowater_relevance4.tif")
  g$water4 = raster::extract(r, g, method = 'simple', fun = median)
  
  # r = raster("distancetowater_relevance5.tif")
  # g$water5 = raster::extract(r, g, method = 'simple', fun = median)
  # 
  # r = raster("distancetowater_relevance6.tif")
  # g$water6 = raster::extract(r, g, method = 'simple', fun = median)
  
  # northness ####
  r = raster("proj_dem_northness_30m.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g$northness = raster::extract(r, g, method = 'simple', fun = median)
  
  # eastness ####
  r = raster("proj_dem_eastness_30m.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g$eastness = raster::extract(r, g, method = 'simple', fun = median)
  
  # bark types ####
  r = raster("NSW_stringybark_distribution.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g$stringybark = raster::extract(r, g, method = 'simple', fun = median)
  
  r = raster("NSW_ribboning_distribution.tif")
  g$ribbonbark = raster::extract(r, g, method = 'simple', fun = median)
  
  setwd("/glade/scratch/kjfuller/data/chapter3")
  g = st_transform(g, crs = targetcrs)
  st_write(g, paste0("ch3_forGAMs_poly_prefire", x, "_static.gpkg"), delete_dsn = T)
  # g = st_read(paste0("ch3_forGAMs_poly_prefire", x, "_static.gpkg"))
  
  # LFMC ####
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
  
  # VPD ####
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
  
  st_write(g, paste0("ch3_forGAMs_poly_prefire", x, "_dynamic.gpkg"), delete_dsn = T)
  # g = st_read(paste0("ch3_forGAMs_poly_prefire", x, "_dynamic.gpkg"))
  
  # wind direction ####
  g = st_transform(g, crs = targetcrs)
  g_buffer = st_buffer(g, dist = 100000)
  g_buffer = st_transform(g_buffer, st_crs(wind))
  
  # remove all directions when windspeed == 0; select just the stations for calculating distances
  wind = wind |> 
    filter(windspeed != 0)
  
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
    wind_temp$windstats = nrow(winddir)
    # create distance-based weights and calculate distance-weighted mean and wind impact index
    wind_temp$weight = 1 - (wind_temp$dist)/max(wind_temp$dist+1000)
    g_temp$winddir = weighted.mean(wind_temp$winddir, wind_temp$weight)
    g_temp$wind_var = weighted.mean(wind_temp$wind_var, wind_temp$weight)
    g_temp$windspeed = weighted.mean(wind_temp$windspeed, wind_temp$weight)
    g_temp$windgust = weighted.mean(wind_temp$windgust, wind_temp$weight)
    
    g_temp$winddiff = g_temp$aspect - g_temp$windmean
    g_temp$winddiff = cos(g_temp$winddiff * pi / 180)
    # select relevant variables for passing to final df
    g_temp = st_transform(g_temp, crs = targetcrs)
    
    l[[i]] = g_temp
  }
  g_temp = bind_rows(l)
  g = full_join(g, g_temp)
  
  # write to disk ####
  setwd("/glade/scratch/kjfuller/data/chapter3")
  st_write(g, paste0("ch3_forGAMs_prefire", x, "_poly_allvars.gpkg"), delete_dsn = T)
  
  print(paste0("*******************  ", x, " done  *********************"))
  }, error = function(e){print(x); cat("ERROR :", conditionMessage(e), "\n")})
}

extractfun(7)
extractfun(14)
extractfun(30)
extractfun(60)
extractfun(90)
extractfun(180)

## still need to extract: fire type categories (FESM directly) -> not all isochrons align with FESM fires
## aspect, wind direction done, fire regime types (in order to restrict less representative veg types), number of dwellings, distance to roads and distance to water (take the min of both) done