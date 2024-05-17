library(sf)
library(tidyverse)
library(tidymodels)
library(spThin)
library(car)
library(MASS)
library(UBL)

setwd("E:/chapter3/for GAMs")
g = st_read("isochrons_prep6.gpkg")
nrow(g)
## 58,556

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
                rh98:Gmethod,
                rh98.dist:Gmethod.dist,
                meanffd:ffdi_final,
                ffdi_cat,
                rough.1:slope.9,
                elev.1:elev.9,
                stringybark.1:ribbonbark.9,
                LFMC:winddir,
                windnorth,
                windeast,
                strahler4:strahler11,
                s4:s11,
                strahler4.only:strahler11.only,
                s4.only:s11.only)
nrow(g)
## 58,556
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
## 17,569
write.csv(test, "testingset_isochrons.csv", row.names = F)

train = training(sb)
nrow(train)
## 40,987
write.csv(train, "trainingset_isochrons.csv", row.names = F)

train |> group_by(ffdi_cat) |> tally()
# one:        8975
# two:        18682
# three:      11432
# four:       1898