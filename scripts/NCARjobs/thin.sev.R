library(sf)
library(tidyverse)
library(tidymodels)
library(spThin)
library(car)
library(MASS)
library(UBL)
memory.limit(size=50000)

# thin ####
# setwd("/glade/scratch/kjfuller/data/chapter3")
setwd("E:/chapter3/for GAMs")
thinfun = function(x){
  g = st_read(paste0("ch3_forGAMs_prefire", x, "_final.gpkg"))
  g$fire_reg = as.factor(g$fire_reg)
  targetcrs = st_crs(g)
  g$lon = st_coordinates(g)[,1]
  g$lat = st_coordinates(g)[,2]
  st_geometry(g) = NULL
  
  g$breaks = apply(g |> dplyr::select(firelines, roads), 1, FUN = min, na.rm = T)
  # g$breaks.all2 = apply(g |> dplyr::select(firelines, roads, water2), 1, FUN = min, na.rm = T)
  # g$breaks.all3 = apply(g |> dplyr::select(firelines, roads, water2, water3), 1, FUN = min, na.rm = T)
  ## breaks, breaks.all2 and breaks.all3 are the same, as are water2 and water3
  g$breaks.all = apply(g |> dplyr::select(firelines, roads, water2, water3, water4), 1, FUN = min, na.rm = T)
  
  # g |> group_by(severity) |> tally()
  ## well-distributed, all things considered
  
  l = list()
  for(i in c(1:length(unique(g$fire_reg)))){
    g_temp = g |>
      filter(fire_reg == unique(g$fire_reg)[i])
    l[[i]] = as.data.frame(thin.algorithm(data.frame(long = g_temp$lon, lat = g_temp$lat), thin.par = 1, reps = 1))
    l[[i]] = g |>
      inner_join(l[[i]], by = c("lon" = "Longitude", "lat" = "Latitude"))
  }
  g = bind_rows(l)
  g = st_as_sf(g, coords = c("lon", "lat"), crs = targetcrs)
  st_write(g, paste0("ch3_forGAMs_prefire", x, "_thinned1.gpkg"), delete_dsn = T)
}

thinfun(7)
thinfun(14)
thinfun(30)
thinfun(60)
thinfun(90)
thinfun(180)

# smote by severity ####
smotefun = function(x){
  # setwd("/glade/scratch/kjfuller/data/chapter3")
  setwd("E:/chapter3/for GAMs")
  g = st_read(paste0("ch3_forGAMs_prefire", x, "_thinned1.gpkg"))
  targetcrs = st_crs(g)
  g$lon = st_coordinates(g)[,1]
  g$lat = st_coordinates(g)[,2]
  st_geometry(g) = NULL
  
  g = g %>%
    dplyr::select(severity,
                  # shot_number,
                  # ID,
                  # veg,
                  fire_reg,
                  rh98:over_cover,
                  LFMC,
                  VPD,
                  maxtemp:maxwd,
                  ffdi_final,
                  ffdi_cat,
                  winddir:windgust.9,
                  winddiff.iso,
                  winddiff.bom,
                  category,
                  # elevation,
                  slope,
                  stringybark,
                  ribbonbark,
                  firelines,
                  roads,
                  # water2,
                  water3,
                  water4,
                  breaks,
                  breaks.all,
                  house.density,
                  lon,
                  lat)
  g$fire_reg = as.factor(g$fire_reg)
  g$severity = as.factor(g$severity)
  g$category = as.factor(g$category)
  g = na.omit(g)
  
  g$ffdi_cat = factor(g$ffdi_cat, levels = c("one",
                                             "two",
                                             "three",
                                             "four"))
  
  for(i in c(1:length(unique(g$severity)))){
    g1 = g |> 
      filter(severity == unique(g$severity)[i])
    g1$severity = 1
    g2 = g |> 
      filter(severity != unique(g$severity)[i])
    g2$severity = 0
    g1 = rbind(g1, g2)
    g1$severity = as.factor(g1$severity)
    
    sb = g1 %>%
      initial_split(strata = severity, prop = 7/10, seed = 5)
    test = testing(sb)
    print("nrow(test) = ")
    print(nrow(test))
    test = st_as_sf(test, coords = c("lon", "lat"), crs = targetcrs)
    st_write(test, paste0("testingdata_prefire", x, "_sev", i, ".gpkg"), delete_dsn = T)
    train = training(sb)
    print("nrow(train) = ")
    print(nrow(train))
    train1 = st_as_sf(train, coords = c("lon", "lat"), crs = targetcrs)
    st_write(train1, paste0("trainingdata_prefire", x, "_sev", i, ".gpkg"), delete_dsn = T)
    
    # ns = train |> group_by(ffdi_cat) |> tally()
    # ref = median(ns$n)
    # ref
    # ## x = 180: 10,232
    # ## x = 90: 4,563
    # ## x = 60: 2,522
    # ## x = 30: 967
    # ## x = 14: 109
    # ## x = 7: 11
    # 
    # # calculate the ratio for SMOTE'ing based on the median number of observations in all FFDI categories, with Extreme fires resampled x2
    # # C.list = list(one = ref/ns$n[ns$ffdi_cat == "one"], two = ref/ns$n[ns$ffdi_cat == "two"], three = ref/ns$n[ns$ffdi_cat == "three"], four = 2) 
    
    ## not bad, will be able to resample to balanced
    
    smote <- SmoteClassif(ffdi_cat ~ .,
                          train,
                          dist = "HEOM", C.perc = "balance", k = 3)
    smote <- SmoteClassif(severity ~ .,
                          smote,
                          dist = "HEOM", C.perc = "balance", k = 3)
    smote = st_as_sf(smote, coords = c("lon", "lat"), crs = targetcrs)
    st_write(smote, paste0("ch3_forGAMs_prefire", x, "_smotesev", i, ".gpkg"), delete_dsn = T)
  }
}

# smotefun(7)
smotefun(14)
smotefun(30)
smotefun(60)
smotefun(90)
smotefun(180)


# smote overall ####
smotefun = function(x){
  # setwd("/glade/scratch/kjfuller/data/chapter3")
  setwd("E:/chapter3/for GAMs")
  g = st_read(paste0("ch3_forGAMs_prefire", x, "_thinned1.gpkg"))
  targetcrs = st_crs(g)
  g$lon = st_coordinates(g)[,1]
  g$lat = st_coordinates(g)[,2]
  st_geometry(g) = NULL
  
  g = g %>%
    dplyr::select(severity,
                  # shot_number,
                  # ID,
                  # veg,
                  fire_reg,
                  rh98:over_cover,
                  LFMC,
                  VPD,
                  maxtemp:maxwd,
                  ffdi_final,
                  ffdi_cat,
                  winddir:windgust.9,
                  winddiff.iso,
                  winddiff.bom,
                  category,
                  # elevation,
                  slope,
                  stringybark,
                  ribbonbark,
                  firelines,
                  roads,
                  # water2,
                  water3,
                  water4,
                  breaks,
                  breaks.all,
                  house.density,
                  lon,
                  lat)
  g$fire_reg = as.factor(g$fire_reg)
  g$severity = as.factor(g$severity)
  g$category = as.factor(g$category)
  g = na.omit(g)
  
  g$ffdi_cat = factor(g$ffdi_cat, levels = c("one",
                                             "two",
                                             "three",
                                             "four"))
  
  g$severity = as.factor(g$severity)
  
  sb = g %>%
    initial_split(strata = severity, prop = 7/10, seed = 5)
  test = testing(sb)
  print("nrow(test) = ")
  print(nrow(test))
  test = st_as_sf(test, coords = c("lon", "lat"), crs = targetcrs)
  st_write(test, paste0("testingdata_prefire", x, "_sev.gpkg"), delete_dsn = T)
  train = training(sb)
  print("nrow(train) = ")
  print(nrow(train))
  train1 = st_as_sf(train, coords = c("lon", "lat"), crs = targetcrs)
  st_write(train1, paste0("trainingdata_prefire", x, "_sev.gpkg"), delete_dsn = T)
  
  # ns = train |> group_by(ffdi_cat) |> tally()
  # ref = median(ns$n)
  # ref
  # ## x = 180: 10,232
  # ## x = 90: 4,563
  # ## x = 60: 2,522
  # ## x = 30: 967
  # ## x = 14: 109
  # ## x = 7: 11
  # 
  # # calculate the ratio for SMOTE'ing based on the median number of observations in all FFDI categories, with Extreme fires resampled x2
  # # C.list = list(one = ref/ns$n[ns$ffdi_cat == "one"], two = ref/ns$n[ns$ffdi_cat == "two"], three = ref/ns$n[ns$ffdi_cat == "three"], four = 2) 
  
  ## not bad, will be able to resample to balanced
  
  smote <- SmoteClassif(ffdi_cat ~ .,
                        train,
                        dist = "HEOM", C.perc = "balance", k = 3)
  smote <- SmoteClassif(severity ~ .,
                        smote,
                        dist = "HEOM", C.perc = "balance", k = 3)
  smote = st_as_sf(smote, coords = c("lon", "lat"), crs = targetcrs)
  st_write(smote, paste0("ch3_forGAMs_prefire", x, "_smotesev.gpkg"), delete_dsn = T)
}

# smotefun(7)
smotefun(14)
smotefun(30)
smotefun(60)
smotefun(90)
smotefun(180)

