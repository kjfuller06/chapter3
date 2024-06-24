library(sf)
library(tidyverse)
library(tidymodels)
library(spThin)
library(car)
library(MASS)
library(UBL)

x = 180
setwd("E:/chapter3/for GAMs")
g = st_read(paste0("ch3_forGAMs_poly_prefire", x, "_final_redo_2-15.gpkg"))
g = g |> filter(fire_reg < 16)
g$fire_reg = as.factor(g$fire_reg)
targetcrs = st_crs(g)
st_geometry(g) = NULL

g$breaks = apply(g |> dplyr::select(firelines, roads), 1, FUN = min, na.rm = T)

g = g %>%
  dplyr::select(prog,
                fire_reg,
                rh98:fhd_normal.9,
                maxtemp:maxwd,
                ffdi_final,
                ffdi_cat,
                elevation_sd,
                elevation_se,
                stringybark,
                stringybark.1,
                stringybark.9,
                ribbonbark,
                ribbonbark.1,
                ribbonbark.9,
                LFMC,
                LFMC.1,
                LFMC.9,
                VPD,
                VPD.1,
                VPD.9,
                winddir,
                winddir.stdev,
                windspeed,
                windspeed.1,
                windspeed.9,
                windspeed.stdev,
                windgust,
                windgust.9,
                windgust.stdev,
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
write.csv(test, paste0("testingdata_prefire", x, "_iso_redo_2-15.csv"), row.names = F)

train = training(sb)
print("nrow(train) = ")
print(nrow(train))
write.csv(train, paste0("trainingdata_prefire", x, "_iso_redo_2-15.csv"), row.names = F)

ns = train |> group_by(ffdi_cat) |> tally()
ref = median(ns$n)
## x = 180: 450
