# load data and libraries ####
library(raster)
library(sf)
library(tidyverse)
library(lubridate)
library(exactextractr)
library(terra)

# setwd("/glade/scratch/kjfuller/data/GEDI")
# f3 = st_read("g2_filter3.gpkg")
# nrow(f3)
# ## 2,187,542
# min(f3$Date, na.rm = T)
# max(f3$Date, na.rm = T)
# f3 = f3 |>
#   filter(Date < "2020-04-01")
# min(f3$Date, na.rm = T)
# max(f3$Date, na.rm = T)
# nrow(f3)
# ## 1,357,336
# 
# st_write(f3, "ch3_f3_daterestricted.gpkg", delete_dsn = TRUE)
# gedi = st_read("ch3_f3_daterestricted.gpkg")
# targetcrs = st_crs(gedi)
# 
# # limit shots to minimally managed forests
# setwd("/glade/scratch/kjfuller/data")
# land1 = st_read("proj_NSW_landuse_Class1.1.shp") %>%
#   st_transform(crs = targetcrs)
# 
# land2 = st_read("proj_NSW_landuse_Class1.2.shp") %>%
#   st_transform(crs = targetcrs)
# 
# land3 = st_read("proj_NSW_landuse_Class1.3.shp") %>%
#   st_transform(crs = targetcrs)
# 
# gedi1 = gedi[land1, ]
# gedi2 = gedi[land2, ]
# gedi3 = gedi[land3, ]
# gedi = rbind(gedi1, gedi2, gedi3)
# gedi = unique(gedi)
# 
setwd("/glade/scratch/kjfuller/data/GEDI")
# st_write(gedi, "ch3_f4.gpkg", delete_dsn = T)
gedi = st_read("ch3_f4.gpkg")
targetcrs = st_crs(gedi)

# load fire history variables
setwd("/glade/scratch/kjfuller/data/fireyears")
lastfire = rast("2016lastfireyear_1970.tif")
int.min = rast("fireinterval_min_1970.tif")
int.max = rast("fireinterval_max_1970.tif")
nfires = rast("2016numberoffires_1970.tif")

# extract and calculate fire history metrics ####
g_buffer = st_buffer(gedi, dist = 12.5)
g_buffer = st_transform(g_buffer, crs = st_crs(lastfire))
gedi = st_transform(gedi, crs = st_crs(lastfire))

g_temp = exact_extract(lastfire, g_buffer, fun = "mode", append_cols = TRUE)
names(g_temp)[names(g_temp) == "mode"] = "lastfire.hist"
gedi = left_join(gedi, g_temp)

g_temp = exact_extract(int.min, g_buffer, fun = "mode", append_cols = TRUE)
names(g_temp)[names(g_temp) == "mode"] = "int.min"
gedi = left_join(gedi, g_temp)

g_temp = exact_extract(int.max, g_buffer, fun = "mode", append_cols = TRUE)
names(g_temp)[names(g_temp) == "mode"] = "int.max"
gedi = left_join(gedi, g_temp)

g_temp = exact_extract(nfires, g_buffer, fun = "mode", append_cols = TRUE)
names(g_temp)[names(g_temp) == "mode"] = "nfires"
g_temp$nfires[is.na(g_temp$nfires)] = 0
gedi = left_join(gedi, g_temp)

gedi = st_transform(gedi, crs = targetcrs)

## calculate TSF based on historical fire date- replace TSF with historical where severity == 0 and NA where there was no fire in the records at all
gedi$month = month(gedi$Date)
gedi$season = NA
gedi$season[gedi$month > 6] = "current"
gedi$season[gedi$month < 7] = "previous"
gedi$fireyear = as.numeric(substr(gedi$Date, 1, 4))
## determine equivalent fire year of shot data collection to match historical fire year data
gedi$fireyear[gedi$season == "previous"] = gedi$fireyear[gedi$season == "previous"] - 1
gedi$TSF.hist = gedi$fireyear - gedi$lastfire.hist

setwd("/glade/scratch/kjfuller/data/GEDI_fhist")
st_write(gedi, "ch3_f4_fhist.gpkg", delete_dsn = TRUE)