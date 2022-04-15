library(raster)
library(sf)
library(tidyverse)
library(geojsonsf)
library(scales)
library(moments)

# extract fire dates ####
setwd("E:/chapter2/GEDI_FESM")
gedi = st_read("g2_f5_allfires_impacts_allmetrics.gpkg")
target_crs = st_crs(gedi)

# extract fire dates ####
setwd("E:/chapter3/")
nbr = list.files("FESM_RdNBR", full.names = TRUE)
fesm = list.files("FESM_json")

nbr_md = data.frame(id = rep(NA, length(nbr)),      
                    crs = rep(NA, length(nbr)),
                    fireSD = rep(NA, length(nbr)),
                    fireED = rep(NA, length(nbr)),
                    shotSD = rep(NA, length(nbr)),
                    shotED = rep(NA, length(nbr)),
                    shotnum = rep(NA, length(nbr)),
                    fireclass = rep(NA, length(nbr)),
                    firecause = rep(NA, length(nbr)),
                    firecategory = rep(NA, length(nbr)))

for(i in c(1:length(fesm))){
  tryCatch({
    setwd("E:/chapter3/")
    
    # identify crs using text-parsing
    t = jsonlite::read_json(paste0("FESM_json/",fesm[i]))
    s = t$crs$properties$name
    s = substr(s, 23, 27)
    # load geojson
    sf1 = geojsonsf::geojson_sf(paste0("FESM_json/", fesm[i]))
    sf1 = st_as_sf(sf1)

    # assign crs manually
    st_crs(sf1) = as.numeric(s)

    # store metadata in nbr_md df
    nbr_md$id[i] = unique(sf1$IncidentId)
    nbr_md$crs[i] = s
    nbr_md$fireSD[i] = unique(sf1$StartDate)
    nbr_md$fireED[i] = unique(sf1$EndDate)
    nbr_md$fireclass[i] = unique(sf1$FireClass)
    nbr_md$firecause[i] = unique(sf1$FireCauseId)
    nbr_md$firecategory[i] = unique(sf1$FireTypeCategoryId)

    # load raster
    r = list.files("FESM_RdNBR", pattern = unique(sf1$IncidentId))
    r = raster(paste0("FESM_RdNBR/", r), band = 4)

    # create raster extent polygon
    e <- extent(r)
    p <- as(e, 'SpatialPolygons') %>% st_as_sf()
    st_crs(p) = st_crs(r)

    # select GEDI shots and extract fire severity, remove NaNs and convert to crs of veg file
    g = st_transform(gedi, crs = st_crs(r))
    g = g[p,]
    g = cbind(g, RdNBR = raster::extract(r, st_coordinates(g), method = 'simple'))
    g = g %>% 
      filter(!is.na(RdNBR))
    g = st_transform(g, crs = target_crs)
    g$nbr_id = unique(sf1$IncidentId)

    # store shot start and end dates in nbr_md
    nbr_md$shotSD[i] = min(g$Date, na.rm = TRUE)
    nbr_md$shotED[i] = max(g$Date, na.rm = TRUE)
    nbr_md$shotnum[i] = nrow(g)
    # write file
    setwd("/glade/scratch/kjfuller/data/GEDI_FESM")
    if(nrow(g) > 0){
      st_write(g, paste0("g2_fesm_RdNBR_", unique(sf1$IncidentId), ".gpkg"), delete_dsn = TRUE)
    }
  }, error = function(e){print(i); cat("ERROR :", conditionMessage(e), "\n")})
}
write.csv(nbr_md, "fesm_nbr_metadata.csv", row.names = FALSE)

# scaling raster values
hist(r, xlim = c(-500, 1500), breaks = 200, xlab = "RdNBR", main = "Original RdNBR value distribution")

skewness(r)
rnew = values(r)
scalefun = function(x){(x-min(x))/(max(x)-min(x))}
r0 = scalefun(rnew)*99 + 1
r1 = scalefun(rnew)*99 + 101
r2 = scalefun(rnew)*99 + 201
r3 = scalefun(rnew)*99 + 301
r4 = scalefun(rnew)*99 + 401
rnew = data.frame(val = c(r0, r1, r2, r3, r4))

hist(rnew$val, breaks = 200, xlab = "RdNBR", main = "Scaled RdNBR value distribution")

# example distribution
rnew = rnorm(1000, mean = 0, sd = 1)
r0 = scalefun(rnew)*99 + 1
rnew = rnorm(1000, mean = 0, sd = 1)
r1 = scalefun(rnew)*99 + 101
rnew = rnorm(1000, mean = 0, sd = 1)
r2 = scalefun(rnew)*99 + 201
rnew = rnorm(1000, mean = 0, sd = 1)
r3 = scalefun(rnew)*99 + 301
rnew = rnorm(1000, mean = 0, sd = 1)
r4 = scalefun(rnew)*99 + 401
rnew = data.frame(val = c(r0, r1, r2, r3, r4))

hist(rnew$val, breaks = 200, xlab = "Scaled RdNBR value", main = "New fire severity variable distribution")
