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

setwd("E:/chapter3/for GAMs")
x = 180
modelnom = paste0("ch3_GAM_iso_prefire", x, "_redo")
# g = read.csv(paste0("ch3_forGAMs_poly_prefire", x, "_smote.csv"))
# g = read.csv(paste0("trainingdata_prefire", x, "_iso.csv"))
g = read.csv(paste0("trainingdata_prefire", x, "_iso_redo_1-27.csv"))
g$ffdi_cat = as.factor(g$ffdi_cat)
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

set.seed(5)
gam1 = bam(logprog ~ 
             # fire_reg + 
             # s(rh98) + 
             # s(over_cover) + 
             s(cover_z_1, k = 3) + 
             # s(cover, k = 3) + 
             # s(fhd_normal) + 
             s(elevation_sd, k = 200) +
             # stringybark + ## 4,093.758 as only
             # stringybark.1 + ## 4,110.637 as only
             # stringybark.9 + ## 4,114.606 as only
             # ribbonbark + ## 4,128.259 as only
             ribbonbark.1 +
             # ribbonbark.9 +
             s(LFMC.1, k = 80) +
             # s(LFMC.9) +
             # s(LFMC) +
             # s(VPD.9) +
             # s(VPD.1) + 
             s(VPD, k = 342) +  
             s(winddir.stdev, k = 15) +
             s(windspeed.stdev, k = 15) + 
             # s(windspeed) + 
             # s(windspeed.1) + 
             # s(windspeed.9) + 
             # s(windgust.stdev, k = 15) + 
             # s(windgust) +
             # s(windgust.9) + 
             s(water2, k = 340),
           data = g, 
           method = "fREML")
AIC(logLik.gam(gam1))

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
# fvisgam(gam1, view = c("cover_z_1", "LFMC.1"))
# vis.gam(gam1, view = c("windspeed.stdev", "wind.stdev"), theta = 35, phi = 32, n.grid = 100)

setwd("D:/chapter3/outputs/GAMs")
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
p1 = plot_smooth(gam1, view = "cover_z_1", n.grid = 1000, transform = exp)
p1 = plot_smooth(gam1, view = "elevation_sd", n.grid = 1000, transform = exp)
p1 = plot_smooth(gam1, view = "stringybark", n.grid = 1000, transform = exp)
p1 = plot_smooth(gam1, view = "LFMC.1", n.grid = 1000, transform = exp)
p1 = plot_smooth(gam1, view = "VPD", n.grid = 1000, transform = exp)
p1 = plot_smooth(gam1, view = "winddir.stdev", n.grid = 1000, transform = exp)
p1 = plot_smooth(gam1, view = "windspeed.stdev", n.grid = 1000, transform = exp)
p1 = plot_smooth(gam1, view = "water2", n.grid = 1000, transform = exp)
dev.off()

vals = data.frame(variable = c("cover_z_1",
                               "elevation_sd",
                               "stringybark",
                               "LFMC.1",
                               "VPD",
                               "winddir.stdev",
                               "windspeed.stdev",
                               "water2"),
                  median = c(0.394663512706757,
                             40.6508745071107,
                             0.673750013113022,
                             68.7860080404862,
                             3.21212732791901,
                             43.8821637351754,
                             4.34749883984023,
                             8700))
write.csv(vals, paste0(modelnom, "_medianvaluesforplotting.csv"), row.names = F)

setwd("E:/chapter3/for GAMs")
# test = read.csv(paste0("testingdata_prefire", x, "_iso.csv"))
test = read.csv(paste0("testingdata_prefire", x, "_iso_redo.csv"))
test$logprog = log(test$prog)

preds<-predict(gam1, type="response", newdata=test,
               se.fit=TRUE)

test$prediction = preds$fit
test$pred.se = preds$se.fit

test$error = test$prediction - test$logprog
R2 = 1 - (sum((test$logprog - test$prediction)^2)/sum((test$logprog - mean(test$logprog))^2))
R2

setwd("D:/chapter3/outputs/GAMs")
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

