library(raster)
library(tidyverse)
library(tmap)

setwd("E:/chapter3/from Rachael/R project/DFMC/DFMC/DFMC_rasters/2020/2020")
dfmc = list.files()
dfmc1 = raster(dfmc[[1]])
dfmc1

tmap_mode("view")
tm_shape(dfmc1) + tm_raster()

setwd("E:/chapter3/from Rachael/R project/LFMC/LFMC/GEE_LFMC/LFMC_rasters/GEE_v20May2020_RevisedMask")
lfmc = list.files()
lfmc1 = raster(lfmc[[1]])
lfmc1

tm_shape(lfmc1) + tm_raster()

lfmc1 = raster(lfmc[[52]])
lfmc1

tm_shape(lfmc1) + tm_raster()
