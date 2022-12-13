library(raster)
library(sf)
library(tidyverse)
library(geojsonsf)

setwd("E:/chapter3")
# setwd("/glade/scratch/kjfuller/data")
fesm = list.files("FESM_json", pattern = ".json")

df = data.frame()
for(i in c(1:length(fesm))){
  # load geojson
  sf1 = geojsonsf::geojson_sf(paste0("FESM_json/", fesm[i]))
  sf1 = st_as_sf(sf1)
  
  st_geometry(sf1) = NULL
  sf1 = sf1 |> 
    dplyr::select(IncidentId,
                  FireCauseId:IncidentReference,
                  IncidentName,
                  EndDate) |> 
    unique()
  names(sf1)[1] = "fire_id"
  
  df = rbind(df, sf1)
}
write.csv(df, "FESM_firetypes.csv", row.names = F)
