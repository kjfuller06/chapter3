library(sf)
library(raster)
library(tidyverse)
library(tidymodels)
library(mgcv)
library(itsadug)

setwd("/glade/scratch/kjfuller/data/chapter3")
# setwd("E:/chapter3/for GAMs")
x = 180
modelnom = paste0("ch3_GAM_iso_prefire", x)
g = read.csv(paste0("ch3_forGAMs_poly_prefire", x, "_smote.csv"))
g$log_string = log(g$stringybark + 1)
g$log_ribbon = log(g$ribbonbark + 1)

g = g |> 
  filter(prog < 10) |> 
  filter(wind.stdev < 130) |> 
  filter(ribbonbark > 0) |> 
  filter(stringybark > 0)

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
gam1 = bam(prog ~
             # s(rh98) +
             # s(cover_z_1) +
             # s(over_cover) +
             # s(fhd_normal) +
             # s(slope) +
             # s(maxtemp) +
             s(maxrh) +
             # s(maxws) +
             s(ffdi_final) +
             s(elevation_sd) +
             log_string +
             log_ribbon +
             # s(LFMC) +
             s(VPD) +
             s(wind.stdev) +
             # s(windspeed) +
             # s(windgust) +
             # s(winddiff.iso) +
             # s(winddiff.bom) +
             # s(firelines) +
             # s(roads) +
             s(water2),
             # s(water3) +
             # s(water4) +
             # s(breaks) +
             # s(breaks.all2) +
             # s(breaks.all3) +
             # s(breaks.all4) +
             # s(house.density),
           data = train,
           method = "fREML")
summary(gam1)

set.seed(5)
gam2 = bam(sqrt(prog) ~
                    # s(rh98) +
                    # s(cover_z_1) +
                    # s(over_cover) +
                    # s(fhd_normal) +
                    # slope +
                    # s(maxtemp) +
                    # s(maxrh) +
                    # maxws +
                    # s(ffdi_final) +
                    s(elevation_sd, k = 80) +
                    s(stringybark) +
                    # s(ribbonbark) +
             s(stringybark, by = ribbonbark) +
                    # log_string +
                    # log_ribbon +
                    # s(LFMC) +
                    VPD,
                    # slope +
                    # wind.stdev,
                    # s(wind.stdev, by = slope),
                    # s(windspeed) +
                    # windgust +
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
     shift = coef(gam1)[1])

vis.gam(gam2, view = c("stringybark", "ribbonbark"), n.grid = 50,
        ticktype = "detailed", color = "topo")

par(mfrow = c(3, 3))
# p1 = plot_smooth(gam1, view = "slope", n.grid = 1000)
p1 = plot_smooth(gam2, view = "slope", n.grid = 1000, ylim = c(0, 10))

# p1 = plot_smooth(gam1, view = "maxws", n.grid = 1000)
# p1 = plot_smooth(gam2, view = "maxws", n.grid = 1000)

# p1 = plot_smooth(gam1, view = "elevation_sd", n.grid = 1000)
p1 = plot_smooth(gam2, view = "elevation_sd", n.grid = 1000, ylim = c(0, 10))

p1 = plot_smooth(gam2, view = "stringybark", n.grid = 1000, ylim = c(0, 10))

# p1 = plot_smooth(gam1, view = "VPD", n.grid = 1000)
p1 = plot_smooth(gam2, view = "VPD", n.grid = 1000, ylim = c(0, 10))

# p1 = plot_smooth(gam1, view = "wind.stdev", n.grid = 1000)
p1 = plot_smooth(gam2, view = "wind.stdev", n.grid = 1000, ylim = c(0, 10))

# p1 = plot_smooth(gam1, view = "windgust", n.grid = 1000)
# p1 = plot_smooth(gam2, view = "windgust", n.grid = 1000)

p1 = plot_smooth(gam2, view = "winddiff.iso", n.grid = 1000, ylim = c(0, 10))

# p1 = plot_smooth(gam1, view = "water2", n.grid = 1000)
# p1 = plot_smooth(gam2, view = "water2", n.grid = 1000)

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
gam.check(gam1)
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
# preds<-predict(gam1, type="response", newdata=test,
#                se.fit=TRUE,
#                cluster = cl)
# stopCluster(cl)
# 
# test$prediction = preds$fit
# test$pred.se = preds$se.fit
# 
# test$error = test$prediction - test$response
# R2 = 1 - (sum((test$response - test$prediction)^2)/sum((test$response - mean(test$response))^2))
# 
# print("test R^2 = ")
# print(R2)
# 
# R2adj <- 1- ((1 - R2) * (length(test$response) - 1)/
#                (length(test$response) - length(gam1$coefficients) - 1))
# 
# print("test adjR^2 = ")
# print(R2adj)

# # plot
# jpeg(paste0(modelnom, "_effectplots.jpeg"), width = 3000, height = 2000, res = 200, units = "px")
# plot(gam1, pages = 1,
#      all.terms = T,
#      rug = T,
#      se = T,
#      shade = T,
#      shift = coef(gam1)[1])
# dev.off()
