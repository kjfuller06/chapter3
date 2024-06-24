library(sf)
library(tidyverse)
library(tidymodels)
library(spThin)
library(car)
library(MASS)
library(UBL)

setwd("E:/chapter3/for GAMs")
g = st_read("isochrons_prep2.gpkg")
nrow(g)
## 5,025

g$fire_reg = as.factor(g$fire_reg)
targetcrs = st_crs(g)
st_geometry(g) = NULL

g = g %>%
  dplyr::select(area,
                fireline,
                progtime,
                poly_sD,
                poly_eD,
                prog,
                logprog,
                spread,
                logspread,
                fire_reg,
                rh98.12.5:fhd_normal.12.5,
                ffdi_final,
                ffdi_cat,
                abs.slope,
                rough.1:w.s.speed.mag,
                stringybark.1:ribbonbark.9,
                LFMC:winddir2,
                s4.only:s11.only,
                s4.cumu:s10.cumu)
nrow(g)
## 5,025
g$ffdi_cat = factor(g$ffdi_cat, levels = c("one",
                                           "two",
                                           "three",
                                           "four"))
write.csv(g, "modelingset_isochrons.csv", row.names = F)

set.seed(15)
sb = g %>%
  initial_split(strata = spread, prop = 7/10, seed = 15)
test = testing(sb)
nrow(test)
## 1,509
write.csv(test, "testingset_isochrons.csv", row.names = F)

train = training(sb)
nrow(train)
## 3,516
write.csv(train, "trainingset_isochrons.csv", row.names = F)

train |> group_by(ffdi_cat) |> tally()
# one:        382
# two:        1,375
# three:      1,367
# four:       392