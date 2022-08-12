library(raster)
library(sf)
library(tidyverse)
library(geojsonsf)
library(exactextractr)

setwd("E:/chapter2/GEDI_fhist")
gedi = st_read("g2_f4_fhist.gpkg")
targetcrs = st_crs(gedi)

setwd("E:/chapter3")
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
  # load RdNBR raster
  r3 = list.files("FESM_RdNBR", pattern = unique(sf1$IncidentId))
  r3 = raster(paste0("FESM_RdNBR/", r3[1]), band = 4)
  r2 = raster(r3)
  values(r2) = values(r3)
  rm(r3)
  
  # create raster extent polygon
  e <- extent(r)
  p <- as(e, 'SpatialPolygons') %>% st_as_sf()
  st_crs(p) = st_crs(r)
  
  # select GEDI shots and extract fire severity, remove NaNs and convert to crs of veg file
  g = st_transform(gedi, crs = st_crs(r))
  g = g[p,]
  g_buffer = st_buffer(g, dist = 12.5)
  
  g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
  names(g_temp)[names(g_temp) == "mode"] = "severity"
  g = left_join(g, g_temp)
  
  g = st_transform(g, crs = st_crs(r2))
  g_buffer = st_buffer(g, dist = 12.5)
  
  g_temp = exact_extract(r2, g_buffer, fun = "mode", append_cols = TRUE)
  names(g_temp)[names(g_temp) == "mode"] = "RdNBR"
  g = left_join(g, g_temp)
  
  # select only shots that intersect with fires- keep severity == 0 in case I need to use these as a control
  g = g %>% 
    filter(!is.na(severity))
  g = st_transform(g, crs = targetcrs)
  g$fire_eD = unique(sf1$EndDate)
  g$fire_id = unique(sf1$IncidentId)
  
  # process dates
  g$fire_eD = as.POSIXct(strptime(g$fire_eD, format = "%Y%m%d"))
  g = g %>% 
    filter(Date < fire_eD)
  g$lon = st_coordinates(g)[,1]
  g$lat = st_coordinates(g)[,2]
  st_geometry(g) = NULL
  return(g)
}
g = extFESMfun(num)

for(i in c((num + 1):(end))){
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
st_write(gedi_fires, paste0("ch3_g2_f3_shotsandFESM_fires_", code, ".gpkg"), delete_dsn = TRUE)
st_write(gedi_nofires, paste0("ch3_g2_f3_shotsandFESM_nofires_", code, ".gpkg"), delete_dsn = TRUE)