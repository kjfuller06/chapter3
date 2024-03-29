library(sf)
library(tidyverse)
library(tidymodels)
library(spThin)
library(car)
library(MASS)
library(UBL)

# setwd("/glade/scratch/kjfuller/data/chapter3")
# thinfun = function(x){
x = 180
setwd("E:/chapter3/for GAMs")
# g = st_read(paste0("ch3_forGAMs_poly_prefire", x, "_final.gpkg"))
# g = st_read(paste0("ch3_forGAMs_poly_prefire", x, "_final_redo_1-27.gpkg"))
# g = st_read(paste0("ch3_forGAMs_poly_prefire", x, "_final_redo_2-14.gpkg"))
g = st_read(paste0("ch3_forGAMs_poly_prefire", x, "_final_redo_2-15.gpkg"))
g = g |> filter(fire_reg < 16)
g$fire_reg = as.factor(g$fire_reg)
targetcrs = st_crs(g)
st_geometry(g) = NULL

g$breaks = apply(g |> dplyr::select(firelines, roads), 1, FUN = min, na.rm = T)
# g$breaks.all3 = apply(g |> dplyr::select(firelines, roads, hydro3), 1, FUN = min, na.rm = T)
# g$breaks.all4 = apply(g |> dplyr::select(firelines, roads, hydro3, hydro4), 1, FUN = min, na.rm = T)
# g$breaks.all5 = apply(g |> dplyr::select(firelines, roads, hydro3, hydro4, hydro5), 1, FUN = min, na.rm = T)
# g$breaks.all6 = apply(g |> dplyr::select(firelines, roads, hydro3, hydro4, hydro5, hydro6), 1, FUN = min, na.rm = T)
# g$breaks.all7 = apply(g |> dplyr::select(firelines, roads, hydro3, hydro4, hydro5, hydro6, hydro7), 1, FUN = min, na.rm = T)

g = g %>%
  dplyr::select(prog,
                fire_reg,
                rh98:fhd_normal.9,
                # slope,
                # slope_min,
                # slope_max,
                maxtemp:maxwd,
                ffdi_final,
                ffdi_cat,
                # ID,
                elevation_sd,
                stringybark,
                stringybark.1,
                stringybark.9,
                # stringybark_min,
                # stringybark_max,
                ribbonbark,
                ribbonbark.1,
                ribbonbark.9,
                # ribbonbark_min,
                # ribbonbark_max,
                LFMC,
                LFMC.1,
                LFMC.9,
                # LFMC_min,
                # LFMC_max,
                VPD,
                VPD.1,
                VPD.9,
                # VPD_min,
                # VPD_max,
                winddir,
                winddir.stdev,
                windspeed,
                windspeed.1,
                windspeed.9,
                windspeed.stdev,
                # windspeed_min,
                # windspeed_max,
                windgust,
                windgust.9,
                windgust.stdev,
                # windgust_max,
                maxtemp,
                maxrh,
                maxws,
                winddiff.iso,
                winddiff.bom,
                firelines,
                roads,
                hydro1:hydro8.9,
                breaks)
g$fire_reg = as.factor(g$fire_reg)
g = na.omit(g)
g$ffdi_cat = factor(g$ffdi_cat, levels = c("one",
                                           "two",
                                           "three",
                                           "four"))

set.seed(15)
sb = g %>%
  initial_split(strata = prog, prop = 7/10, seed = 15)
test = testing(sb)
print("nrow(test) = ")
print(nrow(test))
# write.csv(test, paste0("testingdata_prefire", x, "_iso.csv"), row.names = F)
# write.csv(test, paste0("testingdata_prefire", x, "_iso_redo_1-27.csv"), row.names = F)
# write.csv(test, paste0("testingdata_prefire", x, "_iso_redo_2-14.csv"), row.names = F)
write.csv(test, paste0("testingdata_prefire", x, "_iso_redo_2-15.csv"), row.names = F)

train = training(sb)
print("nrow(train) = ")
print(nrow(train))
# write.csv(train, paste0("trainingdata_prefire", x, "_iso.csv"), row.names = F)
# write.csv(train, paste0("trainingdata_prefire", x, "_iso_redo_1-27.csv"), row.names = F)
# write.csv(train, paste0("trainingdata_prefire", x, "_iso_redo_2-14.csv"), row.names = F)
write.csv(train, paste0("trainingdata_prefire", x, "_iso_redo_2-15.csv"), row.names = F)

ns = train |> group_by(ffdi_cat) |> tally()
ref = median(ns$n)
## x = 180: 450
## x = 90: 220
## x = 60: 152
## x = 30: 42
## x = 14: 9
## x = 7: 4

# calculate the ratio for SMOTE'ing based on the median number of observations in all FFDI categories, with Extreme fires resampled x2
C.list = list(one = ref/ns$n[ns$ffdi_cat == "one"], two = ref/ns$n[ns$ffdi_cat == "two"], three = ref/ns$n[ns$ffdi_cat == "three"], four = 2) 
memory.limit(size=50000)
smote <- SmoteClassif(ffdi_cat ~ .,
                      train,
                      dist = "HEOM", C.perc = C.list, k = 3)
smote |> group_by(ffdi_cat) |> tally()
print(nrow(smote))
# write.csv(smote, paste0("ch3_forGAMs_poly_prefire", x, "_smote_1-27.csv"), row.names = F)
# write.csv(smote, paste0("ch3_forGAMs_poly_prefire", x, "_smote_2-14.csv"), row.names = F)
write.csv(smote, paste0("ch3_forGAMs_poly_prefire", x, "_smote_2-15.csv"), row.names = F)
# }

# thinfun(7) ## "low" is absent from dataset
# thinfun(14)
# thinfun(30)
# thinfun(60)
# thinfun(90)
# thinfun(180)

# plots ####
setwd("E:/chapter3/for GAMs")
train = read.csv(paste0("trainingdata_prefire", x, "_iso_redo_2-15.csv"))
test = read.csv(paste0("testingdata_prefire", x, "_iso_redo_2-15.csv"))
all = rbind(train, test)
nrow(all)
