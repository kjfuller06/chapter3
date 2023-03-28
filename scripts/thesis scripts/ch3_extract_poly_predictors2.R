library(sf)
library(raster)
library(tidyverse)
library(exactextractr)
library(rgeos)

# start ####
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

setwd("/glade/scratch/kjfuller/data/chapter3")
# setwd("E:/chapter3/for GAMs")
g = st_read("ch3_forGAMs_poly_prefire180_structure.gpkg")
targetcrs = st_crs(g)
g = g |>
  dplyr::select(ID,
                poly_sD,
                poly_eD)

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
  if(nrow(lfmc_temp) > 1){
    print(paste0("number of rasters = ", nrow(lfmc_temp)))
    r_temp = raster(lfmc_temp$files[nrow(lfmc_temp)])
    for(v in c(nrow(lfmc_temp):2)){
      r_mask = raster(lfmc_temp$files[v-1])
      r_temp = overlay(r_temp, r_mask, fun = function(x, y) {x[is.na(x)] <- y[is.na(x)]; x})
    }
    l = raster::extract(r_temp, g_temp, method = 'simple')
    g_temp$LFMC = median(unlist(l), na.rm = T)
    g_temp$LFMC.1 = quantile(unlist(l), probs = 0.1, na.rm = T)
    g_temp$LFMC.9 = quantile(unlist(l), probs = 0.9, na.rm = T)
  } else {
    print("only 1 LFMC raster")
    r = raster(lfmc_temp$files[1])
    l = raster::extract(r, g_temp, method = 'simple')
    g_temp$LFMC = median(unlist(l), na.rm = T)
    g_temp$LFMC.1 = quantile(unlist(l), probs = 0.1, na.rm = T)
    g_temp$LFMC.9 = quantile(unlist(l), probs = 0.9, na.rm = T)
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

      l = raster::extract(r, g_temp2, method = 'simple')
      g_temp2$LFMC = median(unlist(l), na.rm = T)
      g_temp2$LFMC.1 = quantile(unlist(l), probs = 0.1, na.rm = T)
      g_temp2$LFMC.9 = quantile(unlist(l), probs = 0.9, na.rm = T)
      
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

setwd("/glade/scratch/kjfuller/data/chapter3")
# setwd("E:/chapter3/for GAMs")
st_write(g, "ch3_forGAMs_poly_prefire180_LFMC.gpkg", delete_dsn = T)
# g = st_read("ch3_forGAMs_poly_prefire180_LFMC.gpkg")
targetcrs = st_crs(g)

# VPD ####
setwd("/glade/scratch/kjfuller/data/VPD")
# setwd("E:/chapter3/from Rachael/VPD")
r = raster(vpd$files[1])
crs(r) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
## res 0.05
g = st_transform(g, crs = st_crs(CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")))
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
      crs(r_temp) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
      l[[v]] = raster::extract(r_temp, g_temp, method = 'simple')
    }
    g_temp$VPD = median(unlist(l), na.rm = T)
    g_temp$VPD.1 = quantile(unlist(l), probs = 0.1, na.rm = T)
    g_temp$VPD.9 = quantile(unlist(l), probs = 0.9, na.rm = T)
  } else {
    r = raster(vpd_temp$files[1])
    crs(r) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
    l = raster::extract(r, g_temp, method = 'simple')
    g_temp$VPD = median(unlist(l), na.rm = T)
    g_temp$VPD.1 = quantile(unlist(l), probs = 0.1, na.rm = T)
    g_temp$VPD.9 = quantile(unlist(l), probs = 0.9, na.rm = T)
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
g = left_join(g, g_temp) |>
  st_transform(crs = targetcrs)

setwd("/glade/scratch/kjfuller/data/chapter3")
# setwd("E:/chapter3/for GAMs")
st_write(g, "ch3_forGAMs_poly_prefire180_dynamic.gpkg", delete_dsn = T)
