library(raster)
library(sf)
library(tidyverse)
library(tmap)

setwd("E:/chapter3/FESM_ag5jsons_20190325_20210217")
fesm = list.files()

sf1 = read_sf(fesm[1])

setwd("E:/chapter3/FESM_ag1_20190325_20210217")
fesmrast = list.files()

r = raster(paste0("cvmsre_frfs0", sf1$IncidentId[1], "_", sf1$EndDate[1], "_ag1m5.img"))

## does sf1 already contain fire severity info?
## unknown variables in sf1:
#  1) FireTypeCategoryId
#  2) FireCauseId
#  3) FireClass

setwd("E:/chapter3/")
fesm = list.files("FESM_ag5jsons_20190325_20210217")

readfun = function(f){
  sf1 <<- read_sf(paste0("FESM_ag5jsons_20190325_20210217/", fesm[f]))
  r = list.files("FESM_ag1_20190325_20210217", pattern = sf1$IncidentId[])
  r <<- raster(paste0("FESM_ag1_20190325_20210217/", r))
}
readfun(1)
tmap_mode("view")
tm_shape(r) + tm_raster()

# extract fire severity from "r" and EndDate from "sf1"