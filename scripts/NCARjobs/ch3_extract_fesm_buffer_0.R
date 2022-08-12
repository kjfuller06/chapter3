code = "001"
library(raster)
library(sf)
library(tidyverse)
library(geojsonsf)
library(exactextractr)

setwd("/glade/scratch/kjfuller/scripts/chapter3")
source("extract_fesm_function.R")

setwd("/glade/scratch/kjfuller/data/GEDI")
gedi = st_read("g2_f4_fhist.gpkg")
targetcrs = st_crs(gedi)

setwd("/glade/scratch/kjfuller/data")
fesm = list.files("FESM_json", pattern = ".json")
g = extFESMfun(1)

for(i in c((1 + 1):(1 + 99))){
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
setwd("/glade/scratch/kjfuller/data/chapter3")
st_write(gedi_fires, paste0("ch3_g2_f3_shotsandFESM_fires_", code, ".gpkg"), delete_dsn = TRUE)
st_write(gedi_nofires, paste0("ch3_g2_f3_shotsandFESM_nofires_", code, ".gpkg"), delete_dsn = TRUE)