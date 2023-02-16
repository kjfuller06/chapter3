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
modelnom = paste0("ch3_GAM_iso_prefire", x, "_redo_2-15")
# modelnom = paste0("ch3_GAM_iso_prefire", x, "_redo_2-14")
# modelnom = paste0("ch3_GAM_iso_prefire", x, "_redo_1-27")
# g = read.csv(paste0("ch3_forGAMs_poly_prefire", x, "_smote.csv"))
# g = read.csv(paste0("trainingdata_prefire", x, "_iso.csv"))
# g = read.csv(paste0("trainingdata_prefire", x, "_iso_redo_1-27.csv"))
# g = read.csv(paste0("trainingdata_prefire", x, "_iso_redo_2-14.csv"))
g = read.csv(paste0("trainingdata_prefire", x, "_iso_redo_2-15.csv"))
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

set.seed(10)
gam1 = bam(logprog ~ 
             # fire_reg +
             # s(rh98) + ## 62.9
             # s(rh98.1) + ## 64.2
             # s(rh98.9) + ## ns
             # s(over_cover) + ## 62.6
             # s(over_cover.1) + ## 64.4
             # s(over_cover.9) + ## 62.6
             # s(cover_z_1, k = 3) + ## 62.8
             s(cover_z_1.1, k = 3) + ## 64.5
             # s(cover_z_1.9, k = 3) + ## 62.8
             # s(cover, k = 3) + ## 62.8
             # s(cover.1, k = 3) + ## 64.5
             # s(cover.9, k = 3) + ## 62.8
             # s(fhd_normal) + ## 62.8
             # s(fhd_normal.1) + ## 64.2
             # s(fhd_normal.9) + ## 62.3
             s(elevation_sd, k = 200) +
             stringybark + ## 64.5
             # stringybark.1 + ## 64.3
             # stringybark.9 + ## 63.7
             # ribbonbark + ## ns
             # ribbonbark.1 + ## ns
             # ribbonbark.9 + ## ns
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
             # s(hydro1)+ ## 5.85; r2 = 63.9; relationship complex
             # s(hydro2)+ ## 50.58; r2 = 63.6; negative relationship
             # s(hydro3)+ ## 6.97; r2 = 64.2; relaionship complex, mostly negative
             # s(hydro4), ## 2.76; r2 = 63.9; relationship mostly positive
             # s(hydro5)+ ## 3.45; r2 = 63.7; mostly positive
             # s(hydro6)+ ## 5.62; r2 = 63.9; mostly positive
             # s(hydro7)+ ## 4.43; r2 = 64.6; the right look
             # s(hydro8), ## 6.73; r2 = 65.3; humped
           # s(hydro1.1)+ 
           #   s(hydro2.1)+
           #   s(hydro3.1)+
             # s(hydro4.1), ## ns
           #   s(hydro5.1)+
           #   s(hydro6.1)+
           #   s(hydro7.1)+
           #   s(hydro8.1),
           # s(hydro1.9)+
           #   s(hydro2.9)+ 
           #   s(hydro3.9)+ 
             s(hydro4.9), ## 64.5; 5.74
             # s(hydro5.9)+ 
             # s(hydro6.9)+ 
             # s(hydro7.9)+ 
             # s(hydro8.9), 
           data = g, 
           method = "fREML")
summary(gam1)
# par(mfrow = c(3, 3))
# p1 = plot_smooth(gam1, view = "hydro1", n.grid = 500, transform = exp, ylim = c(0, 0.5)) ## 66.5; 5.29
# p1 = plot_smooth(gam1, view = "hydro2", n.grid = 500, transform = exp, ylim = c(0, 0.5)) ## 67; 1.86 (ns), 9.06
# p1 = plot_smooth(gam1, view = "hydro3", n.grid = 500, transform = exp, ylim = c(0, 0.5)) ## 68.1; 1.44 (ns), 9.29, 4.42
# p1 = plot_smooth(gam1, view = "hydro4", n.grid = 500, transform = exp, ylim = c(0, 0.5)) ## 68.7; 1.43 (ns), 8.06, 4.39, 3.51
# p1 = plot_smooth(gam1, view = "hydro5", n.grid = 500, transform = exp, ylim = c(0, 0.5)) ## 68.7; 1.32 (ns), 8.14, 4.22, 2.59, 0.32 (ns)
# p1 = plot_smooth(gam1, view = "hydro6", n.grid = 500, transform = exp, ylim = c(0, 0.5)) ## 69; 1.65 (ns), 8.71, 4.26, 2.49, 0.00 (ns), 2.87
# p1 = plot_smooth(gam1, view = "hydro7", n.grid = 500, transform = exp, ylim = c(0, 0.5)) ## 69.7; 1.71 (ns), 20.62, 4.19, 2.56, 0.02 (ns), 2.96, 3.96
# p1 = plot_smooth(gam1, view = "hydro8", n.grid = 500, transform = exp, ylim = c(0, 0.5)) ## 
# summary(gam1)
# par(mfrow = c(3, 3))
# p1 = plot_smooth(gam1, view = "hydro1.1", n.grid = 500, transform = exp, ylim = c(0, 0.5))
# p1 = plot_smooth(gam1, view = "hydro2.1", n.grid = 500, transform = exp, ylim = c(0, 0.5))
# p1 = plot_smooth(gam1, view = "hydro3.1", n.grid = 500, transform = exp, ylim = c(0, 0.5))
# p1 = plot_smooth(gam1, view = "hydro4.1", n.grid = 500, transform = exp, ylim = c(0, 0.5))
# p1 = plot_smooth(gam1, view = "hydro5.1", n.grid = 500, transform = exp, ylim = c(0, 0.5))
# p1 = plot_smooth(gam1, view = "hydro6.1", n.grid = 500, transform = exp, ylim = c(0, 0.5))
# p1 = plot_smooth(gam1, view = "hydro7.1", n.grid = 500, transform = exp, ylim = c(0, 0.5))
# p1 = plot_smooth(gam1, view = "hydro8.1", n.grid = 500, transform = exp, ylim = c(0, 0.5)) ## 69.9, hydro1, 5:7 ns
# summary(gam1)
# par(mfrow = c(3, 3))
# p1 = plot_smooth(gam1, view = "hydro1.9", n.grid = 500, transform = exp, ylim = c(0, 0.5)) 
# p1 = plot_smooth(gam1, view = "hydro2.9", n.grid = 500, transform = exp, ylim = c(0, 0.5))
# p1 = plot_smooth(gam1, view = "hydro3.9", n.grid = 500, transform = exp, ylim = c(0, 0.5))
p1 = plot_smooth(gam1, view = "hydro4.9", n.grid = 500, transform = exp, ylim = c(0, 0.5))
# p1 = plot_smooth(gam1, view = "hydro5.9", n.grid = 500, transform = exp, ylim = c(0, 0.5)) 
# p1 = plot_smooth(gam1, view = "hydro6.9", n.grid = 500, transform = exp, ylim = c(0, 0.5)) 
# p1 = plot_smooth(gam1, view = "hydro7.9", n.grid = 500, transform = exp, ylim = c(0, 0.5)) 
# p1 = plot_smooth(gam1, view = "hydro8.9", n.grid = 500, transform = exp, ylim = c(0, 0.5)) ## 71.9; hydro1, 5, 7 ns
summary(gam1)


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
# saveRDS(gam1, paste0(modelnom, ".rds"))
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
# p1 = plot_smooth(gam1, view = "stringybark", n.grid = 1000, transform = exp)
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
# test = read.csv(paste0("testingdata_prefire", x, "_iso.csv"))
# test = read.csv(paste0("testingdata_prefire", x, "_iso_redo_1-27.csv"))
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

