# some summary stats to run on metadata first, on dates later
library(raster)
library(sf)
library(tidyverse)

# select only fire files that occurred after GEDI data collection ####
setwd("E:/chapter2/GEDI_FESM")
fesm_md = read.csv("fesm_metadata.csv")

# convert dates to as.POSIXct format
fesm_md$fireSD = as.POSIXct(strptime(fesm_md$fireSD, format = "%Y%m%d"))
fesm_md$fireED = as.POSIXct(strptime(fesm_md$fireED, format = "%Y%m%d"))
fesm_md$shotSD = as.Date(fesm_md$shotSD, origin = "1970-01-01")
fesm_md$shotED = as.Date(fesm_md$shotED, origin = "1970-01-01")
head(fesm_md)

# select fires
fesm_md = fesm_md[fesm_md$shotnum != 0,]
fesm_md$diffT = as.numeric(difftime(fesm_md$fireSD, fesm_md$shotSD))
fesm_md = fesm_md %>% 
  filter(diffT > 0)

setwd("E:/chapter3/GEDI_FESM")
write.csv(fesm_md, "fesm_metadata_postGEDI.csv", row.names = FALSE)
