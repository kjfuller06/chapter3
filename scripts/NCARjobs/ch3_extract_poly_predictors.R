library(sf)
library(raster)
library(tidyverse)
library(exactextractr)

# setwd("/glade/scratch/kjfuller/data/LFMC")
setwd("E:/chapter3/from Rachael/R project/LFMC/LFMC/GEE_LFMC/LFMC_rasters/GEE_v20May2020_RevisedMask/")
lfmc = list.files()
lfmc = data.frame(files = lfmc)
lfmc$date = as.Date(substr(lfmc$files, 6, 15), format = "%Y_%m_%d")

# setwd("/glade/scratch/kjfuller/data/VPD")
setwd("E:/chapter3/from Rachael/VPD")
vpd = list.files()
vpd = data.frame(files = vpd)
vpd$date = as.Date(substr(vpd$files, 5, 12), format = "%Y%m%d")

# setwd("/glade/scratch/kjfuller/data/chapter3")
extractfun = function(x){
  setwd("E:/chapter3/GEDI_FESM")
  gedi = st_read(paste0("ch3_isochrons_prefire", x, ".gpkg"))
  st_geometry(gedi) = NULL
  gedi$fire_reg = as.factor(gedi$fire_reg)
  
  setwd("E:/chapter3/isochrons/Progall_ffdi_v3")
  g = st_read("progall_ffdi_v3.shp")
  g$ID = c(1:nrow(g))
  targetcrs = st_crs(g)

  g_agg = g |> 
    right_join(gedi)
  # calculate the median of continuous variables
  g_agg1 = aggregate(data = g_agg, rh98 ~ ID, FUN = median)
  g_agg2 = aggregate(data = g_agg, cover_z_1 ~ ID, FUN = median)
  g_agg3 = aggregate(data = g_agg, over_cover ~ ID, FUN = median)
  g_agg4 = aggregate(data = g_agg, fhd_normal ~ ID, FUN = median)
  
  # calculate the mode of categorical variables
  g_agg5 = aggregate(data = g_agg, fire_reg ~ ID, FUN = function(x){
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  })
  
  g = g |> 
    left_join(g_agg1) |> 
    left_join(g_agg2) |> 
    left_join(g_agg3) |> 
    left_join(g_agg4) |> 
    left_join(g_agg5)
  
  ## need to go back and extract severity data- maybe also go back and use an earlier version of GEDI shots
  
  # load rasters
  # setwd("/glade/scratch/kjfuller/data")
  setwd("D:/chapter1/other_data/Final/terrain variables")

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
  
  # northness
  r = raster("proj_dem_northness_30m.tif")

  g = st_transform(g, crs = st_crs(r))
  g$northnes = raster::extract(r, g, method = 'simple', fun = median)
  
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
