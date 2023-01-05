library(sf)
library(raster)
library(tidyverse)
library(exactextractr)

# LFMC, redo####
setwd("/glade/scratch/kjfuller/data/chapter3")
# setwd("E:/chapter3/for GAMS")
g = st_read(paste0("ch3_forGAMs_prefire180_allvars.gpkg"))
targetcrs = st_crs(g)
g$poly_sD = as.POSIXct(g$lasttim, format = "%Y-%m-%d %H:%M")
g$poly_eD = as.POSIXct(g$time, format = "%Y-%m-%d %H:%M")
g = g |>
  dplyr::select(shot_number,
                ID,
                poly_sD,
                poly_eD)

setwd("/glade/scratch/kjfuller/data/LFMC")
# setwd("E:/chapter3/from Rachael/R project/LFMC/LFMC/GEE_LFMC/LFMC_rasters/GEE_v20May2020_RevisedMask/")
lfmc = list.files()
lfmc = data.frame(files = lfmc)
lfmc$date = as.POSIXct(substr(lfmc$files, 6, 15), format = "%Y_%m_%d")
## using as.Date led to problems for this extraction but the severity data extraction did fine- double-checked

r = raster(lfmc$files[1])
g = st_transform(g, crs = st_crs(r))

index = g |>
  dplyr::select(-ID, -shot_number)
st_geometry(index) = NULL
index = unique(index)
index$index = c(1:nrow(index))
g = left_join(g, index)

ids = unique(g$index)

counter <- 0
l <- list(NULL)
size <- 1
dynamicfun = function(x){
  if( .GlobalEnv$counter == .GlobalEnv$size )
  {length(.GlobalEnv$l) <- .GlobalEnv$size <- .GlobalEnv$size * 2}

  g_temp = g |>
    filter(index == ids[x])
  g_buffer = st_buffer(g_temp, dist = 12.5)

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

  if(nrow(lfmc_temp) > 1){
    print(paste0("number of rasters = ", nrow(lfmc_temp)))
    r_temp = raster(lfmc_temp$files[nrow(lfmc_temp)])
    for(v in c(nrow(lfmc_temp):2)){
      r_mask = raster(lfmc_temp$files[v-1])
      r_temp = overlay(r_temp, r_mask, fun = function(x, y) {x[is.na(x)] <- y[is.na(x)]; x})
    }
    g_temp$LFMC = exact_extract(r_temp, g_buffer, fun = "mode")

  } else {
    print("only 1 LFMC raster")
    r = raster(lfmc_temp$files[1])
    g_temp$LFMC = exact_extract(r, g_buffer, fun = "mode")
  }
  g_temp$LFMC_sD = lfmc_temp$date[1]

  if(any(is.na(g_temp$LFMC))){
    g_temp2 = g_temp |>
      filter(is.na(LFMC))
    g_buffer = st_buffer(g_temp2, dist = 12.5)
    if(min(g_temp2$LFMC_sD, na.rm = T) != min(lfmc$date, na.rm = T)){
      print("LFMC values were NA, attempting to extract earlier LFMC values")
      lfmc_temp = lfmc |>
        filter(date < g_temp2$LFMC_sD[1])
      lfmc_temp = lfmc_temp |>
        filter(date == max(lfmc_temp$date))

      r = raster(lfmc_temp$files[1])

      g_temp2$LFMC = exact_extract(r, g_buffer, fun = "mode")
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
st_write(g, "ch3_forGAMs_prefire180_LFMC.gpkg", delete_dsn = T)

