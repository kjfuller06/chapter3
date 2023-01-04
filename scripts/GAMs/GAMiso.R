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

setwd("E:/chapter3/for GAMs")
x = 180
modelnom = paste0("ch3_GAM_iso_prefire", x)
g = read.csv(paste0("ch3_forGAMs_poly_prefire", x, "_smote.csv"))
g$log_string = log(g$stringybark + 1)
g$log_ribbon = log(g$ribbonbark + 1)
g$log_vpd = log(g$VPD + 1)
g$ffdi_cat = as.factor(g$ffdi_cat)
g$fire_reg = as.factor(g$fire_reg)

g$logprog = log(g$prog)

sb = g %>%
  initial_split(strata = prog, prop = 7/10, seed = 5)
test = testing(sb)
print("nrow(test) = ")
print(nrow(test))
train = training(sb)
print("nrow(train) = ")
print(nrow(train))
rm(sb)

# best_model = glmulti(sqrt(prog) ~ 
#                        rh98 +
#                        cover_z_1 +
#                        over_cover +
#                        fhd_normal +
#                        slope +
#                        ffdi_final +
#                        elevation_sd +
#                        stringybark +
#                        ribbonbark +
#                        LFMC +
#                        wind.stdev +
#                        winddiff.iso +
#                        water2 +
#                        breaks,
#                      data = train,
#                      crit = aicc,
#                      level = 1,
#                      method = "h",
#                      fitfunc = lm,
#                      confsetsize = 100)
# 
# summary(best_model)
# coef(best_model)
# plot(best_model) ## plot the AICc of the top 100 models
# print(best_model) ## decent summary output (2 models within 2 dAICc from the best model)
# weightable(best_model)[1:6,] |> ## pick the top 6 models
#   regulartable() |> ## beautify tables
#   autofit()
# 
# # best model:
# lm1 = lm(sqrt(prog) ~ 
#            over_cover + 
#            slope +
#            # maxws + 
#            ffdi_final + 
#            ffdi_final:maxws +
#            elevation_sd + 
#            # stringybark +
#            # log_string +
#            VPD + 
#            wind.stdev,
#            # breaks.all3,
#          data = train)
# summary(lm1)
# Anova(lm1, type = 3)

set.seed(5)
gam1 = bam(logprog ~ 
             # fire_reg +
             # s(rh98, by = ffdi_cat) +
             # s(cover_z_1, by = ffdi_cat) +
             # s(over_cover, by = ffdi_cat) +
             # s(fhd_normal, by = ffdi_cat) +
             # s(rh98) +
             # s(over_cover) +
             s(cover_z_1) +
             # s(cover_ratio) +
             # s(fhd_normal) +
             # s(maxtemp) +
             # s(maxrh) +
             # s(maxws) +
             # s(ffdi_final, k = 20) + ## FFDI looks funny
             # ffdi_cat +
             elevation_sd +
             s(stringybark, k = 320) +
             # s(stringybark.1) +
             # s(stringybark.9) +
             # log_string +
             # s(ribbonbark) +
             # s(ribbonbark.1) +
             s(ribbonbark.9) +
             # log_ribbon +
             s(LFMC.1) +
             VPD.9 +
             # s(winddiff.iso) +
             # s(windspeed) +
             wind.stdev +
             # windgust +
             # s(windgust, stringybark) +
             # s(windgust, ribbonbark.9) +
             # s(breaks) +
             # s(breaks.all4),
             # s(house.density) +
             s(water2),
           data = train, 
           method = "fREML")
setwd("D:/chapter3/outputs/GAMs")
capture.output(
  paste0("AIC = ", AICc(gam1)),
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

jpeg(paste0(modelnom, "_summedeffectplots.jpeg"), width = 3000, height = 2000, res = 200, units = "px")
par(mfrow = c(3, 3))
p1 = plot_smooth(gam1, view = "cover_z_1", n.grid = 1000)
p1 = plot_smooth(gam1, view = "elevation_sd", n.grid = 1000)
p1 = plot_smooth(gam1, view = "stringybark", n.grid = 1000)
p1 = plot_smooth(gam1, view = "ribbonbark.9", n.grid = 1000)
p1 = plot_smooth(gam1, view = "LFMC.1", n.grid = 1000)
p1 = plot_smooth(gam1, view = "VPD.9", n.grid = 1000)
p1 = plot_smooth(gam1, view = "wind.stdev", n.grid = 1000)
p1 = plot_smooth(gam1, view = "water2", n.grid = 1000)
dev.off()

vals = data.frame(variable = c("cover_z_1",
                               "elevation_sd",
                               "stringybark",
                               "ribbonbark.9",
                               "LFMC.1",
                               "VPD.9",
                               "wind.stdev",
                               "water2"),
                  median = c(0.393526613712311,
                             39.9345141104931,
                             0.670083314180374,
                             0.740256667137146,
                             67.9366415001323,
                             3.39135837554932,
                             39.6745423587811,
                             7990.61962890625))
write.csv(vals, paste0(modelnom, "_medianvaluesforplotting.csv"), row.names = F)

saveRDS(gam1, paste0(modelnom, ".rds"))
# gam1 = readRDS(paste0(modelnom, ".rds"))

preds<-predict(gam1, type="response", newdata=test,
               se.fit=TRUE)

test$prediction = preds$fit
test$pred.se = preds$se.fit

test$error = test$prediction - test$logprog
R2 = 1 - (sum((test$logprog - test$prediction)^2)/sum((test$logprog - mean(test$logprog))^2))

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