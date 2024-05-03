library(raster)
library(sf)
library(tidyverse)
library(geojsonsf)
library(rjson)
library(rgeos)
library(terra)
library(FNN)
library(tmap)
library(viridis)

# terrain roughness ####
setwd("D:/chapter1/data")
elev = raster("proj_dem_s.tif")
rough = raster::terrain(elev, opt = "roughness")
rough
setwd("E:/chapter3")
writeRaster(rough, "terrain_roughness.tif", overwrite = T)

# Strahler stream orders ####
setwd("D:/chapter3/climatedem")
r = raster("strahler2_thin.tif")
df = matrix(c(0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
              2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
              2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
            ncol = 3, byrow = F)
r = reclassify(r, rcl = df)
writeRaster(r, "strahler2_reclass.tif", overwrite = T)

r = raster("strahler2_reclass.tif")
df = matrix(c(0, 11,
              11, 12,
              NA, 1),
            ncol = 3, byrow = F)
r12 = reclassify(r, rcl = df)
r12
writeRaster(r12, "strahler2_reclass12.tif", overwrite = T)
df = matrix(c(0, 10, 11,
              10, 11, 12,
              NA, 1, NA),
            ncol = 3, byrow = F)
r11 = reclassify(r, rcl = df)
r11
# plot(r11)
writeRaster(r11, "strahler2_reclass11.tif", overwrite = T)
df = matrix(c(0, 9, 10,
              9, 10, 12,
              NA, 1, NA),
            ncol = 3, byrow = F)
r10 = reclassify(r, rcl = df)
r10
writeRaster(r10, "strahler2_reclass10.tif", overwrite = T)
df = matrix(c(0, 8, 9,
              8, 9, 12,
              NA, 1, NA),
            ncol = 3, byrow = F)
r9 = reclassify(r, rcl = df)
r9
writeRaster(r9, "strahler2_reclass9.tif", overwrite = T)
df = matrix(c(0, 7, 8,
              7, 8, 12,
              NA, 1, NA),
            ncol = 3, byrow = F)
r8 = reclassify(r, rcl = df)
r8
writeRaster(r8, "strahler2_reclass8.tif", overwrite = T)
df = matrix(c(0, 6, 7,
              6, 7, 12,
              NA, 1, NA),
            ncol = 3, byrow = F)
r7 = reclassify(r, rcl = df)
r7
writeRaster(r7, "strahler2_reclass7.tif", overwrite = T)
df = matrix(c(0, 5, 6,
              5, 6, 12,
              NA, 1, NA),
            ncol = 3, byrow = F)
r6 = reclassify(r, rcl = df)
r6
writeRaster(r6, "strahler2_reclass6.tif", overwrite = T)
df = matrix(c(0, 4, 5,
              4, 5, 12,
              NA, 1, NA),
            ncol = 3, byrow = F)
r5 = reclassify(r, rcl = df)
r5
writeRaster(r5, "strahler2_reclass5.tif", overwrite = T)
df = matrix(c(0, 3, 4,
              3, 4, 12,
              NA, 1, NA),
            ncol = 3, byrow = F)
r4 = reclassify(r, rcl = df)
r4
writeRaster(r4, "strahler2_reclass4.tif", overwrite = T)
df = matrix(c(0, 2, 3,
              2, 3, 12,
              NA, 1, NA),
            ncol = 3, byrow = F)
r3 = reclassify(r, rcl = df)
r3
writeRaster(r3, "strahler2_reclass3.tif", overwrite = T)
df = matrix(c(0, 2,
              2, 12,
              1, NA),
            ncol = 3, byrow = F)
r2 = reclassify(r, rcl = df)
r2
writeRaster(r2, "strahler2_reclass2.tif", overwrite = T)

# GEDI- process shots ####
setwd("E:/chapter2/GEDI old files")
f3 = st_read("g2_filter3.gpkg")
nrow(f3)
## 2,187,542
min(f3$Date, na.rm = T)
max(f3$Date, na.rm = T)
f3 = f3 |>
  filter(Date < "2020-04-01")
min(f3$Date, na.rm = T)
max(f3$Date, na.rm = T)
nrow(f3)
## 1,357,336

st_write(f3, "ch3_f3_daterestricted.gpkg", delete_dsn = TRUE)
gedi = st_read("ch3_f3_daterestricted.gpkg")
targetcrs = st_crs(gedi)

# limit shots to minimally managed forests
setwd("E:/chapter2/land use")
land1 = st_read("proj_NSW_landuse_Class1.1.shp") %>%
  st_transform(crs = targetcrs)

land2 = st_read("proj_NSW_landuse_Class1.2.shp") %>%
  st_transform(crs = targetcrs)

land3 = st_read("proj_NSW_landuse_Class1.3.shp") %>%
  st_transform(crs = targetcrs)

gedi1 = gedi[land1, ]
gedi2 = gedi[land2, ]
gedi3 = gedi[land3, ]
gedi = rbind(gedi1, gedi2, gedi3)
gedi = unique(gedi)

setwd("E:/chapter3")
st_write(gedi, "ch3_f4.gpkg", delete_dsn = T)
gedi = st_read("ch3_f4.gpkg")
targetcrs = st_crs(gedi)

# load fire history variables
setwd("/glade/scratch/kjfuller/data/fireyears")
lastfire = rast("2016lastfireyear_1970.tif")
int.min = rast("fireinterval_min_1970.tif")
int.max = rast("fireinterval_max_1970.tif")
nfires = rast("2016numberoffires_1970.tif")

# extract and calculate fire history metrics
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

setwd("E:/chapter3")
st_write(gedi, "ch3_f4_fhist.gpkg", delete_dsn = TRUE)

# GEDI- extract FESM ####
setwd("E:/chapter2/GEDI old files")
gedi = st_read("ch3_f3_daterestricted.gpkg")
targetcrs = st_crs(gedi)

setwd("E:/chapter3/original")
fesm = list.files("FESM_json", pattern = ".json")

if(num == 701){
  end = length(fesm)
} else {
  end = num + 99
}

extFESMfun = function(x){
  # identify crs using text-parsing
  t = jsonlite::read_json(paste0("FESM_json/",fesm[x]))
  s = t$crs$properties$name
  s = substr(s, 23, 27)
  # load geojson
  sf1 = geojsonsf::geojson_sf(paste0("FESM_json/", fesm[x]))
  sf1 = st_as_sf(sf1)
  
  # assign crs manually
  st_crs(sf1) = as.numeric(s)
  
  # load raster
  r = list.files("FESM_img", pattern = unique(sf1$IncidentId))
  r = raster(paste0("FESM_img/", r[1]))
  
  # create raster extent polygon
  e <- extent(r)
  p <- as(e, 'SpatialPolygons') %>% st_as_sf()
  st_crs(p) = st_crs(r)
  
  # select GEDI shots and extract fire severity, remove NaNs and convert to crs of veg file
  g = st_transform(gedi, crs = st_crs(r))
  g = g[p,]
  
  if(nrow(g) > 0){
    g_buffer = st_buffer(g, dist = 12.5)
    
    g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
    names(g_temp)[names(g_temp) == "mode"] = "severity"
    g = left_join(g, g_temp)
    
    g = st_transform(g, crs = targetcrs)
    
    # select only shots that intersect with fires- keep severity == 0 in case I need to use these as a control
    g = g %>%
      filter(!is.na(severity))
    
    if(nrow(g) > 0){
      g$fire_eD = unique(sf1$EndDate)
      g$fire_sD = unique(sf1$StartDate)
      g$fire_id = unique(sf1$IncidentId)
      
      # process dates
      g$fire_eD = as.POSIXct(strptime(g$fire_eD, format = "%Y%m%d"))
      g$TSF.fesm = difftime(g$Date, g$fire_eD, units = "days")
      g$TSF.fesm = as.numeric(g$TSF.fesm)
      
      g$fire_sD = as.POSIXct(strptime(g$fire_sD, format = "%Y%m%d"))
      g$TUF.fesm = difftime(g$fire_sD, g$Date, units = "days")
      g$TUF.fesm = as.numeric(g$TUF.fesm)
      
      g$lon = st_coordinates(g)[,1]
      g$lat = st_coordinates(g)[,2]
      st_geometry(g) = NULL
    } else {
      g = data.frame()
    }
  } else {
    g = data.frame()
  }
  return(g)
}
g = extFESMfun(num)
while(nrow(g) == 0){
  g = extFESMfun(num)
  num = num + 1
}

for(i in c((num):(end))){
  tryCatch({
    g_temp = extFESMfun(i)
    if(nrow(g_temp) > 0){
      g = g %>%
        full_join(g_temp)
    }
    
    rm(g_temp)
  }, error = function(e){print(i); cat("ERROR :", conditionMessage(e), "\n")})
}
gedi = st_as_sf(g, coords = c("lon", "lat"), crs = targetcrs)
gedi_fires = gedi %>%
  filter(severity != 0)
gedi_nofires = gedi %>%
  filter(severity == 0)
setwd("E:/chapter3/GEDI_FESM")
code = "001"
st_write(gedi_fires, paste0("ch3_shotsandFESM_fires_", code, ".gpkg"), delete_dsn = TRUE)
st_write(gedi_nofires, paste0("ch3_shotsandFESM_nofires_", code, ".gpkg"), delete_dsn = TRUE)

# combine individual files
## fires
setwd("E:/chapter3/GEDI_FESM")
fires = list.files(pattern = "_fires_")
l = list()
a = 1
for(i in c(1:length(fires))){
  l_temp = st_read(fires[[i]])
  if(nrow(l_temp) > 0){
    l[[a]] = l_temp
    a = a + 1
  }
}
fires = bind_rows(l)
st_write(fires, "ch3_shotsandFESM_fires.gpkg", delete_dsn = T)

## no fires
nofires = list.files(pattern = "_nofires_")
l = list()
a = 1
for(i in c(1:length(nofires))){
  l_temp = st_read(nofires[[i]])
  if(nrow(l_temp) > 0){
    l[[a]] = l_temp
    a = a + 1
  }
}
nofires = bind_rows(l)
st_write(nofires, "ch3_shotsandFESM_nofires.gpkg", delete_dsn = T)

# setwd("/glade/scratch/kjfuller/data/GEDI_FESM")
setwd("E:/chapter3/GEDI_FESM")
# select burned sites that were sampled no more than <x> days before fire
# # calculate TSF from FESM data, filter by 180 days
# gedi = st_read("ch3_shotsandFESM_fires.gpkg")
# targetcrs = st_crs(gedi)
# 
# gedi_unburnt = aggregate(data = gedi, TSF.fesm ~ shot_number, FUN = max) %>% # select only shots that burned at least 5 years before
#   filter(TSF.fesm < 0 | TSF.fesm > 1825) 
# gedi_unburnt = gedi %>% 
#   filter(shot_number %in% gedi_unburnt$shot_number) |> 
#   filter(TUF.fesm > 0)
# any(duplicated(gedi_unburnt$shot_number))
# ## TRUE
# 
# dups = gedi_unburnt[duplicated(gedi_unburnt$shot_number),]
# dups = gedi_unburnt |> 
#   filter(shot_number %in% dups$shot_number)
# nrow(dups)
# ## duplicates must be based on multiple fires following shot collection
# 
# gedi_unburnt = gedi_unburnt[order(gedi_unburnt$TUF.fesm),]
# gedi_unburnt$n = NA
# gedi_unburnt$n = with(gedi_unburnt, ave(n, shot_number, FUN = seq_along))
# gedi_unburnt = gedi_unburnt |> 
#   filter(n == 1)
# any(duplicated(gedi_unburnt$shot_number))
# ## FALSE
# 
# st_write(gedi_unburnt, "ch3_shotsandFESM_5yrsunburnt.gpkg")
g_temp = st_read("ch3_shotsandFESM_5yrsunburnt.gpkg")
targetcrs = st_crs(g_temp)
g_temp$lon = st_coordinates(g_temp)[,1]
g_temp$lat = st_coordinates(g_temp)[,2]
st_geometry(g_temp) = NULL

setwd("E:/chapter3")
fhist = st_read("ch3_f4_fhist.gpkg")
st_geometry(fhist) = NULL

setwd("E:/chapter3/GEDI_FESM")
g_temp = left_join(g_temp, fhist) |> 
  filter(TSF.hist >= 5 | is.na(TSF.hist))
g_temp = st_as_sf(g_temp, coords = c("lon", "lat"), crs = targetcrs)
st_write(g_temp, paste0("Ch3_FESMandfhist.gpkg"), delete_dsn = T)

# calculate TSF from FESM data, filter by 180 days
gedi_unburnt = st_read("ch3_shotsandFESM_nofires.gpkg")
targetcrs = st_crs(gedi_unburnt)

any(duplicated(gedi_unburnt$shot_number))
## TRUE

gedi_unburnt = gedi_unburnt[!duplicated(gedi_unburnt$shot_number),]
## remove duplicates- data are only for FESM fires, which did not occur for these shots
any(duplicated(gedi_unburnt$shot_number))
## FALSE
names(gedi_unburnt)
gedi_unburnt$fire_eD = NA
gedi_unburnt$fire_sD = NA
gedi_unburnt$fire_id = NA
gedi_unburnt$TSF.fesm = NA
gedi_unburnt$TUF.fesm = NA

st_write(gedi_unburnt, "ch3_shotsandFESM_unburnt.gpkg")

setwd("E:/chapter3")
fhist = st_read("ch3_f4_fhist.gpkg")
st_geometry(fhist) = NULL

setwd("E:/chapter3/GEDI_FESM")
g_temp = st_read("ch3_shotsandFESM_unburnt.gpkg")
targetcrs = st_crs(g_temp)
g_temp$lon = st_coordinates(g_temp)[,1]
g_temp$lat = st_coordinates(g_temp)[,2]
st_geometry(g_temp) = NULL

g_temp = left_join(g_temp, fhist) |> 
  filter(TSF.hist >= 5 | is.na(TSF.hist))
g_temp = st_as_sf(g_temp, coords = c("lon", "lat"), crs = targetcrs)
g_temp = g_temp |> 
  filter(!shot_number %in% g_fires$shot_number)

g_fires = st_read("Ch3_FESMandfhist.gpkg") |> 
  dplyr::select(-n)
names(g_fires)[names(g_fires) == "geom"] = "geometry"
st_geometry(g_fires) = "geometry"

g_temp = rbind(g_temp, g_fires)
any(duplicated(g_temp$shot_number))

st_write(g_temp, paste0("Ch3_FESMandfhist_forpoly.gpkg"), delete_dsn = T)

# wind ####
setwd("E:/chapter3/from Michael")
bom = st_read("available_100km_stations.shp")
targetcrs = st_crs(bom)
bom$lon = st_coordinates(bom)[,1]
bom$lat = st_coordinates(bom)[,2]
st_geometry(bom) = NULL
any(duplicated(bom$station))
## TRUE
bom = bom |> 
  dplyr::select(station, lon, lat)
bom = bom[!duplicated(bom$station),]

wind1 = read.csv("wind_data_100km_aws_stations.csv")
wind1$DateTime = as.POSIXct(paste0(wind1$date_local, " ", sprintf("%02.0f", wind1$hour_local), ":", sprintf("%02.0f", wind1$min_std)), format = "%Y-%m-%d %H:%M", tz = "UTC")
wind1 = wind1 |> 
  dplyr::select(station,
                DateTime,
                winddir,
                windspeed,
                windgust) |> 
  filter(!is.na(winddir))
wind2 = read.csv("wind_data_100km_synoptic_stations.csv")
wind2$DateTime = as.POSIXct(paste0(wind2$date_local, " ", sprintf("%02.0f", wind2$hour_local), ":", sprintf("%02.0f", wind2$min_std)), format = "%Y-%m-%d %H:%M",  tz = "UTC")
wind2 = wind2 |>
  dplyr::select(station,
                DateTime,
                winddir,
                windspeed) |> 
  filter(!is.na(winddir)) |> 
  filter(!station %in% wind1$station)
wind2$windgust = NA
wind = rbind(wind1, wind2)

wind = wind |> 
  left_join(bom)
wind = st_as_sf(wind, coords = c("lon", "lat"), crs = targetcrs)
st_write(wind, "wind_direction.gpkg", delete_dsn = T)
