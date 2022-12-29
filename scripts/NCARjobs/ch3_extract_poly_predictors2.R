library(sf)
library(raster)
library(tidyverse)
library(exactextractr)
library(rgeos)

setwd("/glade/scratch/kjfuller/data/LFMC")
# setwd("E:/chapter3/from Rachael/R project/LFMC/LFMC/GEE_LFMC/LFMC_rasters/GEE_v20May2020_RevisedMask/")
lfmc = list.files()
lfmc = data.frame(files = lfmc)
lfmc$date = as.POSIXct(substr(lfmc$files, 6, 15), format = "%Y_%m_%d")
## using as.Date led to problems for this extraction but the severity data extraction did fine- double-checked

setwd("/glade/scratch/kjfuller/data/VPD")
# setwd("E:/chapter3/from Rachael/VPD")
vpd = list.files()
vpd = data.frame(files = vpd)
vpd$date = as.POSIXct(substr(vpd$files, 5, 12), format = "%Y%m%d")

# load isochrons ####
setwd("/glade/scratch/kjfuller/data")
# setwd("E:/chapter3/isochrons")
g = st_read("isochrons_8days.gpkg")
targetcrs = st_crs(g)

# LFMC ####
setwd("/glade/scratch/kjfuller/data/LFMC")
# setwd("E:/chapter3/from Rachael/R project/LFMC/LFMC/GEE_LFMC/LFMC_rasters/GEE_v20May2020_RevisedMask/")
r = raster(lfmc$files[1])
g = st_transform(g, crs = st_crs(r))
ids = unique(g$ID)

counter <- 0
l <- list(NULL)
size <- 1
dynamicfun = function(x){
  if( .GlobalEnv$counter == .GlobalEnv$size )
  {length(.GlobalEnv$l) <- .GlobalEnv$size <- .GlobalEnv$size * 2}
  
  g_temp = g |>
    filter(ID == ids[x])
  
  ## select LFMC values that were calculated for dates before the fire started
  lfmc_temp = lfmc |>
    filter(as.numeric(difftime(min(g_temp$poly_sD), date)) > 0)
  ## select the last possible pre-fire LFMC value
  minT = lfmc_temp |>
    filter(date == max(lfmc_temp$date))
  
  ## select LFMC values that were calculated for dates before the fire end
  lfmc_temp = lfmc |> 
    filter(as.numeric(difftime(max(g_temp$poly_eD), date)) > 0)
  ## select the last possible during-fire LFMC value
  maxT = lfmc_temp |>
    filter(date == max(lfmc_temp$date))
  
  lfmc_temp = lfmc |> 
    filter(date >= minT$date & date <= maxT$date)
  
  l = list()
  if(nrow(lfmc_temp) > 0){
    for(v in c(1:nrow(lfmc_temp))){
      r_temp = raster(lfmc_temp$files[v])
      l[[v]] = raster::extract(r_temp, g_temp, method = 'simple')
    }
    g_temp$LFMC = median(unlist(l), na.rm = T)
  } else {
    r = raster(lfmc_temp$files[1])
    g_temp$LFMC = raster::extract(r, g_temp, method = 'simple', fun = median, na.rm = T)
  }
  g_temp$LFMC_sD = lfmc_temp$date[1]
  
  if(any(is.na(g_temp$LFMC))){
    g_temp2 = g_temp |>
      filter(is.na(LFMC))
    if(g_temp2$LFMC_sD != min(lfmc$date)){
      print("LFMC values were NA, attempting to extract earlier LFMC values")
      lfmc_temp = lfmc |>
        filter(date < g_temp2$LFMC_sD[1])
      lfmc_temp = lfmc_temp |>
        filter(date == max(lfmc_temp$date))
      
      r = raster(lfmc_temp$files[1])
      
      g_temp2$LFMC = raster::extract(r, g_temp2, method = 'simple', fun = median, na.rm = T)
      g_temp2$LFMC_sD = lfmc_temp$date[1]
      
      if(any(!is.na(g_temp2$LFMC))){
        print("Earlier LFMC value extracted, at least some values not NA")
        g_temp = g_temp |>
          filter(!is.na(LFMC)) |>
          rbind(g_temp2)
      } else {
        print("Earlier LFMC values were also NA")
      }
    }
  }
  # l[[i]] = g_temp
  
  st_geometry(g_temp) = NULL
  
  .GlobalEnv$counter <- .GlobalEnv$counter + 1
  .GlobalEnv$l[[.GlobalEnv$counter]] <- g_temp
}
for(i in c(1:length(ids))){
  dynamicfun(i)
  print(paste0("LFMC ", i))
}

g_temp = bind_rows(l)
g = left_join(g, g_temp) |> 
  st_transform(crs = targetcrs)

st_write(g, "ch3_forGAMs_poly_LFMC.gpkg", delete_dsn = T)
# g = st_read("ch3_forGAMs_poly_LFMC.gpkg")

# VPD ####
setwd("/glade/scratch/kjfuller/data/VPD")
# setwd("E:/chapter3/from Rachael/VPD")
r = raster(vpd$files[1])
g = st_transform(g, crs = st_crs(4326))
ids = unique(g$ID)

counter <- 0
l <- list(NULL)
size <- 1
dynamicfun = function(x){
  if( .GlobalEnv$counter == .GlobalEnv$size )
  {length(.GlobalEnv$l) <- .GlobalEnv$size <- .GlobalEnv$size * 2}
  g_temp = g |>
    filter(ID == ids[i])
  
  ## select VPD values that were calculated for dates before the fire started
  vpd_temp = vpd |>
    filter(as.numeric(difftime(min(g_temp$poly_sD), date)) >= 0)
  ## select the last possible pre-fire VPD value
  minT = vpd_temp |>
    filter(date == max(vpd_temp$date))
  
  ## select VPD values that were calculated for dates before the fire end
  vpd_temp = vpd |> 
    filter(as.numeric(difftime(max(g_temp$poly_eD), date)) >= 0)
  ## select the last possible during-fire VPD value
  maxT = vpd_temp |>
    filter(date == max(vpd_temp$date))
  
  vpd_temp = vpd |> 
    filter(date >= minT$date & date <= maxT$date)
  
  l = list()
  if(nrow(vpd_temp) > 0){
    for(v in c(1:nrow(vpd_temp))){
      r_temp = raster(vpd_temp$files[v])
      l[[v]] = raster::extract(r_temp, g_temp, method = 'simple')
    }
    g_temp$VPD = median(unlist(l), na.rm = T)
  } else {
    r = raster(vpd_temp$files[1])
    g_temp$VPD = raster::extract(r, g_temp, method = 'simple', fun = median, na.rm = T)
  }
  g_temp$VPD_sD = vpd_temp$date[1]
  
  # l[[i]] = g_temp
  
  st_geometry(g_temp) = NULL
  
  .GlobalEnv$counter <- .GlobalEnv$counter + 1
  .GlobalEnv$l[[.GlobalEnv$counter]] <- g_temp
}

for(i in c(1:length(ids))){
  dynamicfun(i)
  print(paste0("VPD ", i))
}

g_temp = bind_rows(l)
st_geometry(g_temp) = NULL
g = left_join(g, g_temp) |> 
  st_transform(crs = targetcrs)

st_write(g, "ch3_forGAMs_poly_dynamic.gpkg", delete_dsn = T)

## still need to extract: fire type categories (FESM directly) -> not all isochrons align with FESM fires
## aspect, wind direction done, fire regime types (in order to restrict less representative veg types), number of dwellings, distance to roads and distance to water (take the min of both) done