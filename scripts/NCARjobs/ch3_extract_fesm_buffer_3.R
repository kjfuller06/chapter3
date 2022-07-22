code = 301
library(raster)
library(sf)
library(tidyverse)
library(geojsonsf)
library(exactextractr)

# extract fire dates ####
setwd("/glade/scratch/kjfuller/data")
veg = raster("veg_agg.tif")

setwd("/glade/scratch/kjfuller/data/GEDI_fhist")
gedi = st_read("g2_f3_fhist_12.5mbuffer.gpkg")

setwd("/glade/scratch/kjfuller/data")
fesm = list.files("FESM_json", pattern = ".json")
# identify crs using text-parsing
t = jsonlite::read_json(paste0("FESM_json/",fesm[code]))
s = t$crs$properties$name
s = substr(s, 23, 27)
# load geojson
sf1 = geojsonsf::geojson_sf(paste0("FESM_json/", fesm[code]))
sf1 = st_as_sf(sf1)

# assign crs manually
st_crs(sf1) = as.numeric(s)

# load severity classification raster
r = list.files("FESM_img", pattern = unique(sf1$IncidentId))
r = raster(paste0("FESM_img/", r[1]))

# load RdNBR raster
r2 = list.files("FESM_RdNBR", pattern = unique(sf1$IncidentId))
r2 = raster(paste0("FESM_RdNBR/", r[1]), band = 4)

# create raster extent polygon
e <- extent(r)
p <- as(e, 'SpatialPolygons') %>% st_as_sf()
st_crs(p) = st_crs(r)

# select GEDI shots and extract fire severity, remove NaNs and convert to crs of veg file
g = st_transform(gedi, crs = st_crs(r))
g = g[p,]
g_buffer = st_buffer(g, dist = 12.5)

g_temp = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
names(g_temp)[25] = "severity"
g = left_join(g, g_temp)

g_temp = exact_extract(r2, g_buffer, fun = "mode", append_cols = TRUE)
names(g_temp)[25] = "RdNBR"
g = left_join(g, g_temp)

g = g %>% 
  filter(!is.na(severity))
g = st_transform(g, crs = st_crs(veg))
g$fire_sD = unique(sf1$StartDate)
g$fire_id = unique(sf1$IncidentId)

# process dates
g$fire_sD = as.POSIXct(strptime(g$fire_sD, format = "%Y%m%d"))
g = g %>% 
  filter(Date < fire_sD)
targetcrs = st_crs(g)
g$lon = st_coordinates(g)[,1]
g$lat = st_coordinates(g)[,2]
st_geometry(g) = NULL

for(i in c((code + 1):(code + 99))){
  tryCatch({
    # identify crs using text-parsing
    t = jsonlite::read_json(paste0("FESM_json/",fesm[i]))
    s = t$crs$properties$name
    s = substr(s, 23, 27)
    # load geojson
    sf1 = geojsonsf::geojson_sf(paste0("FESM_json/", fesm[i]))
    sf1 = st_as_sf(sf1)

    # assign crs manually
    st_crs(sf1) = as.numeric(s)

    # load severity classification raster
    r = list.files("FESM_img", pattern = unique(sf1$IncidentId))
    r = raster(paste0("FESM_img/", r[1]))
    
    # load RdNBR raster
    r2 = list.files("FESM_RdNBR", pattern = unique(sf1$IncidentId))
    r2 = raster(paste0("FESM_RdNBR/", r[1]), band = 4)

    # create raster extent polygon
    e <- extent(r)
    p <- as(e, 'SpatialPolygons') %>% st_as_sf()
    st_crs(p) = st_crs(r)

    # select GEDI shots and extract fire severity, remove NaNs and convert to crs of veg file
    g_temp = st_transform(gedi, crs = st_crs(r))
    g_temp = g_temp[p,]
    g_buffer = st_buffer(g_temp, dist = 12.5)
    
    g_temp2 = exact_extract(r, g_buffer, fun = "mode", append_cols = TRUE)
    names(g_temp2)[25] = "severity"
    g_temp = left_join(g_temp, g_temp2)
    
    g_temp = st_transform(g_temp, crs = st_crs(r2))
    g_buffer = st_buffer(g_temp, dist = 12.5)
    
    g_temp2 = exact_extract(r2, g_buffer, fun = "mode", append_cols = TRUE)
    names(g_temp2)[25] = "RdNBR"
    g_temp = left_join(g_temp, g_temp2)
    
    g_temp = g_temp %>% 
      filter(!is.na(severity))
    g_temp = st_transform(g_temp, crs = st_crs(veg))
    g_temp$fire_sD = unique(sf1$StartDate)
    g_temp$fire_id = unique(sf1$IncidentId)
    
    # process dates
    g_temp$fire_sD = as.POSIXct(strptime(g_temp$fire_sD, format = "%Y%m%d"))
    g_temp = g_temp %>% 
      filter(Date < fire_sD)
    
    # merge with whole file
    g_temp$lon = st_coordinates(g_temp)[,1]
    g_temp$lat = st_coordinates(g_temp)[,2]
    st_geometry(g_temp) = NULL
    if(nrow(g_temp) > 0){
      g = g %>% 
        full_join(g_temp)
    }
  }, error = function(e){print(i); cat("ERROR :", conditionMessage(e), "\n")})
}
gedi = st_as_sf(g, coords = c("lon", "lat"), crs = targetcrs)
setwd("/glade/scratch/kjfuller/data/GEDI_FESM")
st_write(gedi, paste0("g2_f3_prefireshots_", code, "_12.5mbuffer.gpkg"), delete_dsn = TRUE)
