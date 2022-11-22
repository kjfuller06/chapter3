# args = commandArgs()
# code = substr(args[grepl("code", args)], 6, 10)
# num = as.numeric(substr(args[grepl("num", args)], 5, 10))

library(raster)
library(sf)
library(tidyverse)
library(geojsonsf)
library(exactextractr)

setwd("E:/chapter3/GEDI")
gedi = st_read("ch3_f3_daterestricted.gpkg")
targetcrs = st_crs(gedi)

setwd("E:/chapter3")
fesm = list.files("FESM_json", pattern = ".json")

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
      g$fire_sD = as.POSIXct(strptime(g$fire_sD, format = "%Y%m%d"))
      g = g %>%
        filter(Date < fire_sD)
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
while(nrow(g) == 0){
  g = extFESMfun(num)
  num = num + 1
}

# for(i in c((num):(end))){
#   tryCatch({
    g_temp = extFESMfun(4)
    if(nrow(g_temp) > 0){
      g = g %>%
        full_join(g_temp)
    }

#     rm(g_temp)
#   }, error = function(e){print(i); cat("ERROR :", conditionMessage(e), "\n")})
# }