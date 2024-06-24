# ## note: weather variables all co-vary, including FFDI and VPD, but not LFMC; based on:
# cor(g |> dplyr::select(-ffdi_cat, -fire_reg, -aspect, -firelines, -roads, -winddiff.bom, -water4, -breaks.all4, -house.density, -log_string, -log_ribbon, -log_vpd), method = "spearman")
# cor(g |> dplyr::select(c(maxtemp, maxrh, maxws, maxwd, ffdi_final, stringybark, LFMC, VPD, winddir, wind.stdev, windspeed, windgust, winddiff.iso)), method = "spearman")
# ## all structural variables co-vary, lowest is cover_z_1 and fhd_normal at 0.61
# cor(g |> dplyr::select(rh98, cover_z_1, over_cover, fhd_normal), method = "spearman")

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

# GAM ####
setwd("E:/chapter3/for GAMs")
x = 180
modelnom = paste0("ch3_GAM_iso_prefire", x, "_redo_2-15")
g = read.csv(paste0("trainingdata_prefire", x, "_iso_redo_2-15.csv"))
g$ffdi_cat = as.factor(g$ffdi_cat)
# g = g |> 
#   filter(fire_reg > 1 & fire_reg < 6)
g$fire_reg = as.factor(g$fire_reg)
g$logprog = log(g$prog)
g$stringybark = as.factor(g$stringybark)
g$stringybark[g$stringybark == 0.5] = 1

g$stringybark.1[g$stringybark.1 < 0.5] = 0
g$stringybark.1[g$stringybark.1 >= 0.5] = 1
g$stringybark.1 = as.factor(g$stringybark.1)

g$stringybark.9[g$stringybark.9 < 0.5] = 0
g$stringybark.9[g$stringybark.9 >= 0.5] = 1
g$stringybark.9 = as.factor(g$stringybark.9)

g$ribbonbark = as.factor(g$ribbonbark)
g$ribbonbark[g$ribbonbark == 0.5] = 1

g$ribbonbark.1[g$ribbonbark.1 < 0.5] = 0
g$ribbonbark.1[g$ribbonbark.1 >= 0.5] = 1
g$ribbonbark.1 = as.factor(g$ribbonbark.1)

g$ribbonbark.9[g$ribbonbark.9 < 0.5] = 0
g$ribbonbark.9[g$ribbonbark.9 >= 0.5] = 1
g$ribbonbark.9 = as.factor(g$ribbonbark.9)

set.seed(10)
gam1 = bam(logprog ~ 
             s(cover_z_1.1, k = 15) +
             s(elevation_sd, k = 15) +
             stringybark + 
             s(LFMC.1, k = 15) +
             s(VPD, k = 15) +  
             s(winddir.stdev, k = 15) +
             s(windspeed.stdev, k = 15) +
             s(hydro4.9, k = 15),
             # fire_reg,
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
gam1 = readRDS(paste0(modelnom, ".rds"))

capture.output(
  paste0("AIC = ", AIC(logLik.gam(gam1))),
  file = paste0(modelnom, "modeloutputs.txt"))
capture.output(
  print("**************************summary****************************"),summary(gam1),
  file = paste0(modelnom, "modeloutputs.txt"),
  append = T)
capture.output(
  print("**************************anova******************************"), anova(gam1),
  file = paste0(modelnom, "modeloutputs.txt"),
  append = T)
capture.output(
  print("***************************gam.check**************************"), gam.check(gam1),
  file = paste0(modelnom, "modeloutputs.txt"),
  append = T)
capture.output(
  print("***************************k.check**************************"), k.check(gam1),
  file = paste0(modelnom, "modeloutputs.txt"),
  append = T)

jpeg(paste0(modelnom, "_diagnosticplots.jpeg"), width = 3000, height = 2000, res = 200, units = "px")
par(mfrow = c(2, 2))
gam.check(gam1)
dev.off()

jpeg(paste0(modelnom, "_partialeffectplots.jpeg"), width = 3000, height = 2000, res = 200, units = "px")
plot(gam1, pages = 1,
     all.terms = T,
     rug = T,
     se = T,
     shade = T,
     shift = coef(gam1)[1])
dev.off()

jpeg(paste0(modelnom, "_summedeffectplots.jpeg"), width = 3000, height = 3000, res = 200, units = "px")
par(mfrow = c(3, 3))
p1 = plot_smooth(gam1, view = "cover_z_1.1", n.grid = 1000, transform = exp)
p1 = plot_smooth(gam1, view = "elevation_sd", n.grid = 1000, transform = exp)
p1 = plot_smooth(gam1, view = "LFMC.1", n.grid = 1000, transform = exp)
p1 = plot_smooth(gam1, view = "VPD", n.grid = 1000, transform = exp)
p1 = plot_smooth(gam1, view = "winddir.stdev", n.grid = 1000, transform = exp)
p1 = plot_smooth(gam1, view = "windspeed.stdev", n.grid = 1000, transform = exp)
p1 = plot_smooth(gam1, view = "hydro4.9", n.grid = 1000, transform = exp)
dev.off()

vals = data.frame(variable = c("cover_z_1.1",
                               "elevation_sd",
                               "stringybark",
                               "LFMC.1",
                               "VPD",
                               "winddir.stdev",
                               "windspeed.stdev",
                               "hydro4.9"),
                  median = c(0.312459641695023,
                             39.9345141104931,
                             0,
                             68.4659149951514,
                             3.20854926109314,
                             44.5277772759967,
                             4.3692878048431,
                             12007.6623046875))
write.csv(vals, paste0(modelnom, "_medianvaluesforplotting.csv"), row.names = F)

setwd("E:/chapter3/for GAMs")
test = read.csv(paste0("testingdata_prefire", x, "_iso_redo_2-15.csv"))
test$logprog = log(test$prog)
test$stringybark[test$stringybark == 0.5] = 1
test$stringybark = as.factor(test$stringybark)

preds<-predict(gam1, type="response", newdata=test,
               se.fit=TRUE)

test$prediction = preds$fit
test$pred.se = preds$se.fit

test$error = test$prediction - test$logprog
R2 = 1 - (sum((test$logprog - test$prediction)^2)/sum((test$logprog - mean(test$logprog))^2))
R2

setwd("C:/chapter3/outputs")
capture.output(
  print("************************** R^2 ****************************"), print(paste0("test R^2 = ", R2)),
  file = paste0(modelnom, "modeloutputs.txt"),
  append = T)

R2adj <- 1- ((1 - R2) * (length(test$logprog) - 1)/
               (length(test$logprog) - length(gam1$coefficients) - 1))

capture.output(
  print("************************** adjusted R^2 ****************************"), print(paste0("test adjR^2 = ", R2adj)),
  file = paste0(modelnom, "modeloutputs.txt"),
  append = T)

# MAE = sum(abs(df$obs - df$exp)) * (1/250)
rmse <- exp(sqrt(mean((test$error)^2)))

capture.output(
  print("************************** RMSE ****************************"), print(paste0("RMSE = ", rmse)),
  file = paste0(modelnom, "modeloutputs.txt"),
  append = T)

# partial deviance ####
set.seed(10)
gam.all = bam(logprog ~ 
             s(cover_z_1.1, k = 15) +
             s(elevation_sd, k = 15) +
             stringybark + 
             s(LFMC.1, k = 15) +
             s(VPD, k = 15) +  
             s(winddir.stdev, k = 15) +
             s(windspeed.stdev, k = 15) + 
             s(hydro4.9, k = 15),
           data = g, 
           method = "fREML")
set.seed(10)
gam.null = gam(logprog ~ 1,
               data = g)

# elevation
set.seed(10)
gam.2 = bam(logprog ~ 
              s(cover_z_1.1, k = 15) +
              stringybark + 
              s(LFMC.1, k = 15) +
              s(VPD, k = 15) +  
              s(winddir.stdev, k = 15) +
              s(windspeed.stdev, k = 15) + 
              s(hydro4.9, k = 15),
            sp = gam.all$sp[-2],
            data = g, 
            method = "fREML")

100 - (summary(gam.2)$dev.expl/summary(gam.all)$dev.expl)*100
## 52.1
## 53.16

# understorey cover
set.seed(10)
gam.2 = bam(logprog ~ 
                s(elevation_sd, k = 15) +
                stringybark + 
                s(LFMC.1, k = 15) +
                s(VPD, k = 15) +  
                s(winddir.stdev, k = 15) +
                s(windspeed.stdev, k = 15) + 
                s(hydro4.9, k = 15),
              sp = gam.all$sp[-1],
              data = g, 
              method = "fREML")

100 - (summary(gam.2)$dev.expl/summary(gam.all)$dev.expl)*100
## 5.09
## 4.92

# stringybark
set.seed(10)
gam.2 = bam(logprog ~ 
              s(cover_z_1.1, k = 15) +
              s(elevation_sd, k = 15) +
              s(LFMC.1, k = 15) +
              s(VPD, k = 15) +  
              s(winddir.stdev, k = 15) +
              s(windspeed.stdev, k = 15) + 
              s(hydro4.9, k = 15),
            sp = gam.all$sp,
            data = g, 
            method = "fREML")

100 - (summary(gam.2)$dev.expl/summary(gam.all)$dev.expl)*100
## 1.6
## 1.22

# LFMC
set.seed(10)
gam.2 = bam(logprog ~ 
              s(cover_z_1.1, k = 15) +
              s(elevation_sd, k = 15) +
              stringybark + 
              s(VPD, k = 15) +  
              s(winddir.stdev, k = 15) +
              s(windspeed.stdev, k = 15) + 
              s(hydro4.9, k = 15),
            sp = gam.all$sp[-3],
            data = g, 
            method = "fREML")

100 - (summary(gam.2)$dev.expl/summary(gam.all)$dev.expl)*100
## 2.7
## 2.54

# VPD
set.seed(10)
gam.2 = bam(logprog ~ 
              s(cover_z_1.1, k = 15) +
              s(elevation_sd, k = 15) +
              stringybark + 
              s(LFMC.1, k = 15) +
              s(winddir.stdev, k = 15) +
              s(windspeed.stdev, k = 15) + 
              s(hydro4.9, k = 15),
            sp = gam.all$sp[-4],
            data = g, 
            method = "fREML")

100 - (summary(gam.2)$dev.expl/summary(gam.all)$dev.expl)*100
## 2.6
## 2.41

# wind direction
set.seed(10)
gam.2 = bam(logprog ~ 
              s(cover_z_1.1, k = 15) +
              s(elevation_sd, k = 15) +
              stringybark + 
              s(LFMC.1, k = 15) +
              s(VPD, k = 15) +
              s(windspeed.stdev, k = 15) + 
              s(hydro4.9, k = 15),
            sp = gam.all$sp[-5],
            data = g, 
            method = "fREML")

100 - (summary(gam.2)$dev.expl/summary(gam.all)$dev.expl)*100
## 5.3
## 5.03

# wind speed
set.seed(10)
gam.2 = bam(logprog ~ 
              s(cover_z_1.1, k = 15) +
              s(elevation_sd, k = 15) +
              stringybark + 
              s(LFMC.1, k = 15) +
              s(VPD, k = 15) +  
              s(winddir.stdev, k = 15) +
              s(hydro4.9, k = 15),
            sp = gam.all$sp[-6],
            data = g, 
            method = "fREML")

100 - (summary(gam.2)$dev.expl/summary(gam.all)$dev.expl)*100
## 1.5
## 1.47

# distance to streams
set.seed(10)
gam.2 = bam(logprog ~ 
              s(cover_z_1.1, k = 15) +
              s(elevation_sd, k = 15) +
              stringybark + 
              s(LFMC.1, k = 15) +
              s(VPD, k = 15) +  
              s(winddir.stdev, k = 15) +
              s(windspeed.stdev, k = 15),
            sp = gam.all$sp[-7],
            data = g, 
            method = "fREML")

100 - (summary(gam.2)$dev.expl/summary(gam.all)$dev.expl)*100
## 1.2
## 1.21