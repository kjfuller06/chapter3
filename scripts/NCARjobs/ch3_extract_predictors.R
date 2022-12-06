library(sf)
library(raster)
library(tidyverse)
library(exactextractr)

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

setwd("/glade/scratch/kjfuller/data/chapter3")
# setwd("E:/chapter3/GEDI_FESM")
extractfun = function(x){
  g = st_read(paste0("ch3_isochrons_prefire", x, "_cleaned.gpkg"))
  targetcrs = st_crs(g)
  g = g |> 
    filter(fire_sD > "2019-08-04")
  g$poly_sD = as.Date(g$time)
  
  # load rasters
  setwd("/glade/scratch/kjfuller/data")
  # setwd("D:/chapter1/other_data/Final/terrain variables")

  # elevation
  r = raster("proj_dem_s.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g_buffer = st_buffer(g, dist = 12.5)
  
  g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
  names(g_temp)[names(g_temp) == "mode"] = "elevation"
  g = left_join(g, g_temp)
    
  # northness
  r = raster("proj_dem_northness_30m.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g_buffer = st_buffer(g, dist = 12.5)
  
  g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
  names(g_temp)[names(g_temp) == "mode"] = "northness"
  g = left_join(g, g_temp)
  
  # eastness
  r = raster("proj_dem_eastness_30m.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g_buffer = st_buffer(g, dist = 12.5)
  
  g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
  names(g_temp)[names(g_temp) == "mode"] = "eastness"
  g = left_join(g, g_temp)
  
  # bark types
  r = raster("NSW_stringybark_distribution.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g_buffer = st_buffer(g, dist = 12.5)
  
  g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
  names(g_temp)[names(g_temp) == "mode"] = "stringybark"
  g = left_join(g, g_temp)
  
  r = raster("NSW_ribboning_distribution.tif")
  
  g = st_transform(g, crs = st_crs(r))
  g_buffer = st_buffer(g, dist = 12.5)
  
  g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
  names(g_temp)[names(g_temp) == "mode"] = "ribbonbark"
  g = left_join(g, g_temp)
  
  setwd("/glade/scratch/kjfuller/data/chapter3")
  g = st_transform(g, crs = targetcrs)
  st_write(g, paste0("ch3_forGAMs_prefire", x, "_static.gpkg"), delete_dsn = T)
  # g = st_read(paste0("ch3_forGAMs_prefire", x, "_static.gpkg"))
  
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
    
    lfmc_temp = lfmc |> 
      filter(as.numeric(difftime(min(g_temp$poly_sD), date)) > 0)
    lfmc_temp = lfmc_temp |> 
      filter(date == max(lfmc_temp$date))
    
    r = raster(lfmc_temp$files[1])
    
    g_buffer = st_buffer(g_temp, dist = 12.5)
    
    g_temp2 = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
    names(g_temp2)[names(g_temp2) == "mode"] = "LFMC"
    g_temp2$LFMC_sD = lfmc_temp$date[1]
    g_temp = left_join(g_temp, g_temp2)
    
    if(any(is.na(g_temp$LFMC))){
      g_temp2 = g_temp |> 
        filter(is.na(LFMC))
      lfmc_temp = lfmc |> 
        filter(date < g_temp2$LFMC_sD[1])
      lfmc_temp = lfmc_temp |> 
        filter(date == max(lfmc_temp$date))
      
      r = raster(lfmc_temp$files[1])
      
      g_buffer = st_buffer(g_temp2, dist = 12.5) |> 
        dplyr::select(-LFMC)
      
      g_temp3 = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
      names(g_temp3)[names(g_temp3) == "mode"] = "LFMC"
      g_temp2 = left_join(g_temp2, g_temp3)
      g_temp2$LFMC_sD = lfmc_temp$date[1]
      
      g_temp = g_temp |> 
        filter(!is.na(LFMC)) |> 
        rbind(g_temp2)
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
    
    vpd_temp = vpd |> 
      filter(as.numeric(difftime(min(g_temp$poly_sD), date)) > 0)
    vpd_temp = vpd_temp |> 
      filter(date == max(vpd_temp$date))
    
    r = raster(vpd_temp$files[1])
    
    g_buffer = st_buffer(g_temp, dist = 12.5)
    
    g_temp2 = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
    names(g_temp2)[names(g_temp2) == "mode"] = "VPD"
    g_temp2$VPD_sD = vpd_temp$date[1]
    g_temp = left_join(g_temp, g_temp2)
    
    if(any(is.na(g_temp$VPD))){
      g_temp2 = g_temp |> 
        filter(is.na(VPD))
      vpd_temp = vpd |> 
        filter(date < g_temp2$VPD_sD[1])
      vpd_temp = vpd_temp |> 
        filter(date == max(vpd_temp$date))
      
      r = raster(vpd_temp$files[1])
      
      g_buffer = st_buffer(g_temp2, dist = 12.5) |> 
        dplyr::select(-VPD)
      
      g_temp3 = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
      names(g_temp3)[names(g_temp3) == "mode"] = "VPD"
      g_temp2 = left_join(g_temp2, g_temp3)
      g_temp2$VPD_sD = vpd_temp$date[1]
      
      g_temp = g_temp |> 
        filter(!is.na(VPD)) |> 
        rbind(g_temp2)
    }
    
    l[[i]] = g_temp
    print(paste0("VPD ", i))
  }
  g_temp = bind_rows(l)
  st_geometry(g_temp) = NULL
  g = full_join(g, g_temp)
  
  g = st_transform(g, crs = targetcrs)
  setwd("/glade/scratch/kjfuller/data/chapter3")
  st_write(g, paste0("ch3_forGAMs_prefire", x, "_allvars.gpkg"), delete_dsn = T)
}

extractfun(7)
extractfun(14)
extractfun(30)
extractfun(60)
extractfun(90)
extractfun(180)
