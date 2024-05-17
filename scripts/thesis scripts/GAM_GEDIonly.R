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
g = g |> filter(Gmethod == "median")
nrow(g)
## 2,778

# rh98 ####
set.seed(10)
gam1 = bam(logspread ~ 
             s(rh98),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(rh98),
           data = g, 
           method = "fREML")
summary(gam1)

# cover ####
set.seed(10)
gam1 = bam(logspread ~ 
             s(cover),
           data = g, 
           method = "fREML")
summary(gam1)
## significant; R-adj < 0.02

set.seed(10)
gam1 = bam(logprog ~ 
             s(cover),
           data = g, 
           method = "fREML")
summary(gam1)
## significant; R-adj < 0.03

# cover_z_1 ####
set.seed(10)
gam1 = bam(logspread ~ 
             s(cover_z_1),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(cover_z_1),
           data = g, 
           method = "fREML")
summary(gam1)

# over_cover ####
set.seed(10)
gam1 = bam(logspread ~ 
             s(over_cover),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(over_cover),
           data = g, 
           method = "fREML")
summary(gam1)

# fhd_normal ####
set.seed(10)
gam1 = bam(logspread ~ 
             s(fhd_normal),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(fhd_normal),
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
gam1 = bam(logprog ~ 
             s(LFMC),
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
gam1 = bam(logprog ~ 
             s(VPD),
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
gam1 = bam(logprog ~ 
             s(windspeed),
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
gam1 = bam(logprog ~ 
             s(windgust),
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
             s(windnorth),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logspread ~ 
             s(windeast),
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
             s(windnorth),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(windeast),
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
             s(slope.5),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(rough.5),
           data = g, 
           method = "fREML")
summary(gam1)

# strahler ####
set.seed(10)
gam1 = bam(logspread ~ 
             s(s4),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(s4),
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
gam1 = bam(logprog ~ 
             s(s4.only),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logspread ~ 
             s(s5),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(s5),
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
gam1 = bam(logprog ~ 
             s(s5.only),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logspread ~ 
             s(s6),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(s6),
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
gam1 = bam(logprog ~ 
             s(s6.only),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logspread ~ 
             s(s7),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(s7),
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
gam1 = bam(logprog ~ 
             s(s7.only),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logspread ~ 
             s(s8),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(s8),
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
gam1 = bam(logprog ~ 
             s(s8.only),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logspread ~ 
             s(s9),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(s9),
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
gam1 = bam(logprog ~ 
             s(s9.only),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logspread ~ 
             s(s10),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(s10),
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
gam1 = bam(logprog ~ 
             s(s10.only),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logspread ~ 
             s(s11),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(s11),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logspread ~ 
             s(s11.only),
           data = g, 
           method = "fREML")
summary(gam1)

set.seed(10)
gam1 = bam(logprog ~ 
             s(s11.only),
           data = g, 
           method = "fREML")
summary(gam1)

# best variables- logprog ####
modelnom = "GAM01.prog"
set.seed(10)
gam1 = bam(logprog ~ 
             # vegetation
             s(cover_z_1) +
             ribbonbark.5 +
             stringybark.5 +
             # weather
             s(VPD) +
             s(LFMC) +
             s(windgust) +
             s(windeast) +
             s(windnorth) +
             # terrain
             s(rough.5) +
             s(s11.only),
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
setwd("C:/chapter3/outputs")
saveRDS(gam1, paste0(modelnom, ".rds"))

setwd("C:/chapter3/scripts/thesis scripts")
source('GAM_functions.R')
gamstatfun(modelnom, "logprog")
gamplotdatafun(modelnom)
gamplotfun(modelnom, "logprog")

# best variables- logspread ####
modelnom = "GAM01.spread"
set.seed(10)
gam1 = bam(logspread ~ 
             # vegetation
             s(cover_z_1) +
             ribbonbark.5 +
             stringybark.5 +
             # weather
             s(VPD) +
             s(LFMC) +
             s(windgust) +
             s(windeast) +
             s(windnorth) +
             # terrain
             s(rough.5) +
             s(s4),
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

modelnom = "GAM01.spread"
setwd("C:/chapter3/outputs")
# saveRDS(gam1, paste0(modelnom, ".rds"))

setwd("C:/chapter3/scripts/thesis scripts")
source('GAM_functions.R')
gamstatfun(modelnom, "logspread")
gamplotdatafun(modelnom)
gamplotfun(modelnom, "logspread")
