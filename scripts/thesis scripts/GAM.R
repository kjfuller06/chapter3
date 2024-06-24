# setup ####
library(sf)
library(raster)
library(tidyverse)
library(tidymodels)
library(mgcv)
library(itsadug)
library(glmulti)
library(flextable)
library(car)
library(MuMIn)
library(metR)

setwd("E:/chapter3/for GAMs")
g = read.csv("trainingset_isochrons.csv")
g$ffdi_cat = as.factor(g$ffdi_cat)
g$fire_reg = as.factor(g$fire_reg)
g$stringybark.5 = as.factor(g$stringybark.5)
g$ribbonbark.5 = as.factor(g$ribbonbark.5)

g$ID = c(1:nrow(g))

# rh98 ####
set.seed(10)
gam1 = bam(logspread ~ 
             s(rh98.12.5),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(rh98.12.5),
           data = g, 
           method = "fREML")
summary(gam1)

# cover ####
set.seed(10)
gam1 = bam(logspread ~ 
             s(cover.12.5),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(cover.12.5),
           data = g, 
           method = "fREML")
summary(gam1)

# cover_z_1 ####
set.seed(10)
gam1 = bam(logspread ~ 
             s(cover_z_1.12.5),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(cover_z_1.12.5),
           data = g, 
           method = "fREML")
summary(gam1)

# over_cover ####
set.seed(10)
gam1 = bam(logspread ~ 
             s(over_cover.12.5),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(over_cover.12.5),
           data = g, 
           method = "fREML")
summary(gam1)

# fhd_normal ####
set.seed(10)
gam1 = bam(logspread ~ 
             s(fhd_normal.12.5),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(fhd_normal.12.5),
           data = g, 
           method = "fREML")
summary(gam1)

# stringybark ####
set.seed(10)
gam1 = bam(logspread ~ 
             stringybark.5,
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             stringybark.5,
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logspread ~ 
             stringybark.9,
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             stringybark.9,
           data = g, 
           method = "fREML")
summary(gam1)

# ribbonbark ####
set.seed(10)
gam1 = bam(logspread ~ 
             ribbonbark.5,
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             ribbonbark.5,
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logspread ~ 
             ribbonbark.9,
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             ribbonbark.9,
           data = g, 
           method = "fREML")
summary(gam1)

# fire regime ####
set.seed(10)
gam1 = bam(logspread ~ 
             fire_reg,
           data = g, 
           method = "fREML")
summary(gam1)
anova(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             fire_reg,
           data = g, 
           method = "fREML")
summary(gam1)
anova(gam1)

# FFDI ####
set.seed(10)
gam1 = bam(logspread ~ 
             s(ffdi_final),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logspread ~ 
             ffdi_cat,
           data = g, 
           method = "fREML")
summary(gam1)
anova(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(ffdi_final),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             ffdi_cat,
           data = g, 
           method = "fREML")
summary(gam1)
anova(gam1)

# LFMC ####
set.seed(10)
gam1 = bam(logspread ~ 
             s(LFMC),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logspread ~ 
             s(LFMC.1),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(LFMC),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(LFMC.1),
           data = g, 
           method = "fREML")
summary(gam1)

# VPD ####
set.seed(10)
gam1 = bam(logspread ~ 
             s(VPD),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logspread ~ 
             s(VPD.1),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(VPD),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(VPD.1),
           data = g, 
           method = "fREML")
summary(gam1)

# windspeed ####
set.seed(10)
gam1 = bam(logspread ~ 
             s(windspeed),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logspread ~ 
             s(windspeed.1),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logspread ~ 
             s(windspeed.9),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logspread ~ 
             s(w.s.up.speed),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logspread ~ 
             s(w.s.speed.mag),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(windspeed),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(windspeed.1),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(windspeed.9),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(w.s.up.speed),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(w.s.speed.mag),
           data = g, 
           method = "fREML")
summary(gam1)

# windgust ####
set.seed(10)
gam1 = bam(logspread ~ 
             s(windgust),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logspread ~ 
             s(windgust.9),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(windgust),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(windgust.9),
           data = g, 
           method = "fREML")
summary(gam1)

# winddir ####
set.seed(10)
gam1 = bam(logspread ~ 
             s(winddir),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logspread ~ 
             s(winddir2),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(winddir),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(winddir2),
           data = g, 
           method = "fREML")
summary(gam1)

# roughness ####
set.seed(10)
gam1 = bam(logspread ~ 
             s(rough.5),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(rough.5),
           data = g, 
           method = "fREML")
summary(gam1)

# slope ####
set.seed(10)
gam1 = bam(logspread ~ 
             s(slope),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(slope),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logspread ~ 
             s(abs.slope),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(abs.slope),
           data = g, 
           method = "fREML")
summary(gam1)

# elevation ####
set.seed(10)
gam1 = bam(logspread ~ 
             s(elev.5),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(elev.5),
           data = g, 
           method = "fREML")
summary(gam1)

# strahler ####
set.seed(10)
gam1 = bam(logspread ~ 
             s(s11.only),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logspread ~ 
             s(s10.only),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logspread ~ 
             s(s9.only),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logspread ~ 
             s(s8.only),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logspread ~ 
             s(s7.only),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logspread ~ 
             s(s6.only),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logspread ~ 
             s(s5.only),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logspread ~ 
             s(s4.only),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logspread ~ 
             s(s10.cumu),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logspread ~ 
             s(s9.cumu),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logspread ~ 
             s(s8.cumu),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logspread ~ 
             s(s7.cumu),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logspread ~ 
             s(s6.cumu),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logspread ~ 
             s(s5.cumu),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logspread ~ 
             s(s4.cumu),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(s11.only),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(s10.only),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(s9.only),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(s8.only),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(s7.only),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(s6.only),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(s5.only),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(s4.only),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(s10.cumu),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(s9.cumu),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(s8.cumu),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(s7.cumu),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(s6.cumu),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(s5.cumu),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(s4.cumu),
           data = g, 
           method = "fREML")
summary(gam1)

# best variables- logspread ####
modelnom = "GAM01.spread"
set.seed(10)
gam1 = bam(logspread ~ 
             # vegetation
             s(cover_z_1.12.5) +
             # fire_reg +
             # ribbonbark.5 +
             stringybark.5 +
             # weather
             s(VPD) +
             s(LFMC) +
             s(windspeed) +
             # s(w.s.speed.mag) +
             # terrain
             s(abs.slope) +
             s(s5.cumu),
           data = g, 
           method = "fREML")
summary(gam1)

anova(gam1)
plot(gam1, pages = 1,
     all.terms = T,
     rug = T,
     se = T,
     shade = T,
     shift = coef(gam1)[1])
par(mfrow = c(2, 2))
gam.check(gam1)
k.check(gam1)

g.check = g
g.check$predicted = predict(gam1)
g.check$res = residuals(gam1, type = "deviance")
with(g.check, plot(res ~ logprog))
with(g.check, plot(predicted ~ logprog))
g.check |> filter(predicted == min(g.check$predicted))
## ID = 382
## ID = 116
## ID = 133
## ID = 109
## ID = 1023
## ID = 865
g = g |> filter(ID != 382)
g = g |> filter(ID != 116)
g = g |> filter(ID != 133)
g = g |> filter(ID != 109)
g = g |> filter(ID != 1023)
g = g |> filter(ID != 865)

modelnom = "GAM01.spread"
setwd("C:/chapter3/outputs")
# saveRDS(gam1, paste0(modelnom, ".rds"))
gam1 = readRDS(paste0(modelnom, ".rds"))

setwd("C:/chapter3/scripts/thesis scripts")
source('GAM_functions.R')
gamstatfun(modelnom, "logspread")
gamplotdatafun(modelnom)
gamplotfun(modelnom, "logspread")

# best variables- logprog####
modelnom = "GAM01.prog"
set.seed(10)
gam1 = bam(logprog ~ 
             # vegetation
             s(cover_z_1.12.5) +
             # ribbonbark.5 +
             stringybark.5 +
             # weather
             s(VPD) +
             s(LFMC) +
             s(windspeed) +
             # s(w.s.speed.mag) +
             # terrain
             s(abs.slope) +
             s(s5.cumu),
           data = g, 
           method = "fREML")
summary(gam1)

anova(gam1)
plot(gam1, pages = 1,
     all.terms = T,
     rug = T,
     se = T,
     shade = T,
     shift = coef(gam1)[1])
par(mfrow = c(2, 2))
gam.check(gam1)
k.check(gam1)

g.check = g
g.check$predicted = predict(gam1)
g.check$res = residuals(gam1, type = "deviance")
with(g.check, plot(res ~ logprog))
with(g.check, plot(predicted ~ logprog))
g.check |> filter(predicted == min(g.check$predicted))
## ID = 382
## ID = 116
## ID = 133
## ID = 109
## ID = 1023
## ID = 865
g = g |> filter(ID != 382)
g = g |> filter(ID != 116)
g = g |> filter(ID != 133)
g = g |> filter(ID != 109)
g = g |> filter(ID != 1023)
g = g |> filter(ID != 865)

setwd("C:/chapter3/outputs")
# saveRDS(gam1, paste0(modelnom, ".rds"))
gam1 = readRDS(paste0(modelnom, ".rds"))

setwd("C:/chapter3/scripts/thesis scripts")
source('GAM_functions.R')
gamstatfun(modelnom, "logprog")
gamplotdatafun(modelnom)
gamplotfun(modelnom, "logprog")
