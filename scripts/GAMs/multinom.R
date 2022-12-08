library(sf)
library(raster)
library(tidyverse)
library(tmap)
library(mgcv)

# setwd("E:/chapter3/GEDI_FESM")
setwd("/glade/scratch/kjfuller/data/chapter3")
smotefun = function(x){
  g = st_read(paste0("ch3_forGAMs_prefire", x, "_allvars.gpkg"))
  g = g |> 
    filter(!is.na(LFMC) & !is.na(stringybark) & !is.na(ribbonbark)) |> 
    filter(fire_reg != 7 & fire_reg != 9)
}
g = smotefun(180)
st_geometry(g) = NULL
g$log_string = log(g$stringybark)
g$log_ribbon = log(g$ribbonbark)
g$severity[g$severity == 2] = 0
g$severity[g$severity == 3] = 1
g$severity[g$severity == 4] = 2
g$severity[g$severity == 5] = 3

set.seed(5)
gam1 = gam(list(severity ~
             s(slope) +
             s(northness) +
             s(eastness) +
             log_string +
             log_ribbon +
             s(LFMC) +
             s(VPD) +
             s(rh98) +
             s(cover_z_1) +
             s(over_cover) +
             s(fhd_normal),
             ~ 
               s(slope) +
               s(northness) +
               s(eastness) +
               log_string +
               log_ribbon +
               s(LFMC) +
               s(VPD) +
               s(rh98) +
               s(cover_z_1) +
               s(over_cover) +
               s(fhd_normal),
             ~ 
               s(slope) +
               s(northness) +
               s(eastness) +
               log_string +
               log_ribbon +
               s(LFMC) +
               s(VPD) +
               s(rh98) +
               s(cover_z_1) +
               s(over_cover) +
               s(fhd_normal)),
           data = g,
           family = mgcv::multinom(K = 3))
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

gc()

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

# plot
jpeg(paste0(modelnom, "_effectplots.jpeg"), width = 3000, height = 2000, res = 200, units = "px")
plot(gam1, pages = 1,
     all.terms = T,
     rug = T,
     se = T,
     shade = T,
     shift = coef(gam1)[1])
dev.off()