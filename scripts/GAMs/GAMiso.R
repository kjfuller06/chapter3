## note: median weather variables all co-vary, including FFDI and VPD, but not LFMC; based on:
cor(g |> dplyr::select(-ffdi_cat, -fire_reg, -aspect, -firelines, -roads, -winddiff.bom, -water4, -breaks.all4, -house.density, -log_string, -log_ribbon, -log_vpd), method = "spearman")
cor(g |> dplyr::select(c(maxtemp, maxrh, maxws, maxwd, ffdi_final, stringybark, LFMC, VPD, winddir, wind.stdev, windspeed, windgust, winddiff.iso)), method = "spearman")

library(sf)
library(raster)
library(tidyverse)
library(tidymodels)
library(mgcv)
library(itsadug)
library(glmulti)
library(flextable)
library(car)

setwd("E:/chapter3/for GAMs")
x = 180
modelnom = paste0("ch3_GAM_iso_prefire", x)
g = read.csv(paste0("ch3_forGAMs_poly_prefire", x, "_smote.csv"))
g$log_string = log(g$stringybark + 1)
g$log_ribbon = log(g$ribbonbark + 1)
g$log_vpd = log(g$VPD + 1)

g = g |> 
  filter(prog < 5) |> 
  filter(wind.stdev < 130) |> 
  filter(ribbonbark > 0) |> 
  filter(stringybark > 0) |> 
  filter(ffdi_final < 100)

g$ffdi_cat = NA
g$ffdi_cat[g$ffdi_final <= 12] = "low"
g$ffdi_cat[g$ffdi_final > 12 & g$ffdi_final <= 25] = "high"
g$ffdi_cat[g$ffdi_final > 25 & g$ffdi_final <= 49] = "very high"
g$ffdi_cat[g$ffdi_final > 49 & g$ffdi_final <= 74] = "severe"
g$ffdi_cat[g$ffdi_final > 74 & g$ffdi_final <= 99] = "extreme"
g$ffdi_cat = factor(g$ffdi_cat, levels = c("low",
                                           "high",
                                           "very high",
                                           "severe",
                                           "extreme"))

sb = g %>%
  initial_split(strata = prog, prop = 7/10, seed = 5)
test = testing(sb)
print("nrow(test) = ")
print(nrow(test))
train = training(sb)
print("nrow(train) = ")
print(nrow(train))
rm(sb)

set.seed(5)
gam2 = bam(sqrt(prog) ~
                    # s(rh98) +
                    # s(cover_z_1) +
                    # s(over_cover) +
                    # s(fhd_normal) +
                    slope +
                    # s(maxtemp) +
                    # s(maxrh) +
                    # maxws +
             # slope:maxws +
                    # s(ffdi_final) +
                    elevation_sd +
             # elevation_sd:slope +
                    # s(stringybark) +
                    # s(ribbonbark) +
             # s(stringybark, by = ribbonbark) +
                    # log_string +
                    # log_ribbon +
                    # s(LFMC) +
                    log_vpd,
                    # s(wind.stdev, slope, k = 160),
                    # s(wind.stdev, by = slope),
                    # s(windspeed, k = 160),
                    # s(windspeed, winddir),
                    # s(winddiff.iso, k = 80),
                    # s(winddiff.bom) +
                    # s(firelines) +
                    # s(roads) +
                    # s(water2),
                    # s(water3) +
                    # s(water4) +
                    # s(breaks) +
                    # s(breaks.all2),
                    # s(breaks.all3),
                    # s(breaks.all4) +
                    # s(house.density),
                  data = train,
                  method = "fREML")
summary(gam2)
anova(gam2)

par(mfrow = c(2, 2))
gam.check(gam2)
dev.off()

k.check(gam2)

plot(gam2, pages = 1,
     all.terms = T,
     rug = T,
     se = T,
     shade = T,
     shift = coef(gam2)[1])

par(mfrow = c(2,2))
vis.gam(gam2, view = c("slope", "elevation_sd"), n.grid = 50,
        ticktype = "detailed", color = "topo")

# p1 = plot_smooth(gam1, view = "slope", n.grid = 1000)
p1 = plot_smooth(gam2, view = "slope", n.grid = 1000)
p1 = plot_smooth(gam2, view = "elevation_sd", n.grid = 1000)
p1 = plot_smooth(gam2, view = "log_vpd", n.grid = 1000)

setwd("D:/chapter3/outputs/GAMs")
saveRDS(gam1, paste0(modelnom, ".rds"))

# gam1 = readRDS(paste0(modelnom, ".rds"))

# print("*******************  residuals check *********************")
# gc()
# rsd <- residuals(gam1)
# print(gam(rsd~s(PC1,k=20,bs="cs"),gamma=1.4,data=train))
# print(gam(rsd~s(PC2,k=20,bs="cs"),gamma=1.4,data=train))
# gc()

print("*******************  gam.check *********************")
jpeg(paste0(modelnom, "_diagnosticplots.jpeg"), width = 3000, height = 2000, res = 200, units = "px")
par(mfrow = c(2, 2))
gam.check(gam2)
dev.off()

print("*******************  k.check *********************")
print(k.check(gam1))

print("*******************  summary *********************")
print(summary(gam1))

print("*******************  anova *********************")
print(anova(gam1))

# # extract test R^2
# print("***************** test R^2 values *********************")
# cl = makeCluster(72)
preds<-predict(gam2, type="response", newdata=test,
               se.fit=TRUE
               # ,
               # cluster = cl
               )
# stopCluster(cl)

test$prediction = preds$fit
test$pred.se = preds$se.fit

test$error = test$prediction - test$prog
R2 = 1 - (sum((test$prog - test$prediction)^2)/sum((test$prog - mean(test$prog))^2))

print("test R^2 = ")
print(R2)

R2adj <- 1- ((1 - R2) * (length(test$prog) - 1)/
               (length(test$prog) - length(gam1$coefficients) - 1))

print("test adjR^2 = ")
print(R2adj)

# # plot
# jpeg(paste0(modelnom, "_effectplots.jpeg"), width = 3000, height = 2000, res = 200, units = "px")
# plot(gam1, pages = 1,
#      all.terms = T,
#      rug = T,
#      se = T,
#      shade = T,
#      shift = coef(gam1)[1])
# dev.off()

lm1 = lm(sqrt(prog) ~
             # s(rh98) +
             # s(cover_z_1) +
             # s(over_cover) +
             # s(fhd_normal) +
             slope +
             # s(maxtemp) +
             # s(maxrh) +
             # maxws +
             # slope:maxws +
             # s(ffdi_final) +
             elevation_sd +
             # elevation_sd:slope +
             # s(stringybark) +
             # s(ribbonbark) +
             # s(stringybark, by = ribbonbark) +
             # log_string +
             # log_ribbon +
             # s(LFMC) +
             log_vpd,
           # s(wind.stdev, slope, k = 160),
           # s(wind.stdev, by = slope),
           # s(windspeed, k = 160),
           # s(windspeed, winddir),
           # s(winddiff.iso, k = 80),
           # s(winddiff.bom) +
           # s(firelines) +
           # s(roads) +
           # s(water2),
           # s(water3) +
           # s(water4) +
           # s(breaks) +
           # s(breaks.all2),
           # s(breaks.all3),
           # s(breaks.all4) +
           # s(house.density),
           data = train)
summary(lm1)
plot(lm1)

train = train[-1475,]

lm1 = lm(sqrt(prog) ~
           # s(rh98) +
           # s(cover_z_1) +
           # s(over_cover) +
           # s(fhd_normal) +
           slope +
           # s(maxtemp) +
           # s(maxrh) +
           # maxws +
           # slope:maxws +
           # s(ffdi_final) +
           elevation_sd +
           # elevation_sd:slope +
           # s(stringybark) +
           # s(ribbonbark) +
           # s(stringybark, by = ribbonbark) +
           # log_string +
           # log_ribbon +
           # s(LFMC) +
           log_vpd,
         # s(wind.stdev, slope, k = 160),
         # s(wind.stdev, by = slope),
         # s(windspeed, k = 160),
         # s(windspeed, winddir),
         # s(winddiff.iso, k = 80),
         # s(winddiff.bom) +
         # s(firelines) +
         # s(roads) +
         # s(water2),
         # s(water3) +
         # s(water4) +
         # s(breaks) +
         # s(breaks.all2),
         # s(breaks.all3),
         # s(breaks.all4) +
         # s(house.density),
         data = train)
summary(lm1)
plot(lm1)

best_model = glmulti(sqrt(prog) ~ 
                       rh98 +
                       cover_z_1 +
                       over_cover +
                       fhd_normal +
                       slope +
                       ffdi_final +
                       elevation_sd +
                       stringybark +
                       ribbonbark +
                       LFMC +
                       wind.stdev +
                       winddiff.iso +
                       water2 +
                       breaks,
                     data = train,
                     crit = aicc,
                     level = 1,
                     method = "h",
                     fitfunc = lm,
                     confsetsize = 100)

summary(best_model)
coef(best_model)
plot(best_model) ## plot the AICc of the top 100 models
print(best_model) ## decent summary output (2 models within 2 dAICc from the best model)
weightable(best_model)[1:6,] |> ## pick the top 6 models
  regulartable() |> ## beautify tables
  autofit()

# best model:
lm1 = lm(sqrt(prog) ~ 
           over_cover + 
           slope +
           # maxws + 
           ffdi_final + 
           ffdi_final:maxws +
           elevation_sd + 
           # stringybark +
           # log_string +
           VPD + 
           wind.stdev,
           # breaks.all3,
         data = train)
summary(lm1)
Anova(lm1, type = 3)

train = train |> 
  filter(ffdi_cat != "extreme")
unique(train$ffdi_cat)

set.seed(5)
gam1 = bam(sqrt(prog) ~ 
             over_cover + 
             slope +
             # s(maxws, k = 50) +
             # s(ffdi_final, k = 320) +
             # ffdi_cat +
             # s(maxws, by = ffdi_cat) +
             s(elevation_sd, k = 320) + 
             s(stringybark, k = 20) +
             # s(stringybark, maxws) +
             # s(ribbonbark, maxws, k = 20) +
             # s(log_string, k = 640) +
             VPD +
             wind.stdev +
             s(wind.stdev, stringybark),
             # s(windgust),
             # breaks.all3,
             # water2,
           data = train, 
           method = "fREML")
summary(gam1)
anova(gam1)

par(mfrow = c(2, 2))
gam.check(gam1)
dev.off()

k.check(gam1)

plot(gam1, pages = 1,
     all.terms = T,
     rug = T,
     se = T,
     shade = T,
     shift = coef(gam1)[1])

par(mfrow = c(2, 4))
vis.gam(gam1, view = c("stringybark", "wind.stdev"), theta = 35, phi = 32, n.grid = 50,
        ticktype = "detailed", color = "topo")
p1 = plot_smooth(gam1, view = "over_cover", n.grid = 1000)
p1 = plot_smooth(gam1, view = "slope", n.grid = 1000)
# p1 = plot_smooth(gam1, view = "maxws", n.grid = 1000)
p1 = plot_smooth(gam1, view = "ffdi_final", n.grid = 1000)
p1 = plot_smooth(gam1, view = "elevation_sd", n.grid = 1000)
p1 = plot_smooth(gam1, view = "stringybark", n.grid = 1000)
p1 = plot_smooth(gam1, view = "log_vpd", n.grid = 1000)
p1 = plot_smooth(gam1, view = "wind.stdev", n.grid = 1000)
# p1 = plot_smooth(gam1, view = "windgust", n.grid = 1000)
# p1 = plot_smooth(gam1, view = "breaks", n.grid = 1000)

