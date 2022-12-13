library(sf)
library(raster)
library(tidyverse)
library(tidymodels)
library(mgcv)

# setwd("E:/chapter3/for GAMs")
x = 180
gamfun = function(sev){
  setwd("/glade/scratch/kjfuller/data/chapter3")
  modelnom = paste0("ch3_GAM_severity", sev, "_prefire", x)
  g = st_read(paste0("ch3_forGAMs_prefire", x, "_smotesev", sev, ".gpkg"))
  st_geometry(g) = NULL
  g$log_string = log(g$stringybark)
  g$log_ribbon = log(g$ribbonbark)
  g$severity = as.factor(g$severity)
  g$FireTypeCategoryId = as.factor(g$FireTypeCategoryId)
  
  sb = g %>%
    initial_split(strata = severity, prop = 7/10, seed = 5)
  test = testing(sb)
  print("nrow(test) = ")
  print(nrow(test))
  train = training(sb)
  print("nrow(train) = ")
  print(nrow(train))
  rm(sb)
  
  set.seed(5)
  gam1 = bam(severity ~
               s(elevation, k = 80) +
               s(slope) +
               s(northness, k = 80) +
               s(eastness, k = 80) +
               s(stringybark) +
               s(ribbonbark, k = 80) +
               s(LFMC, k = 160) +
               s(VPD, k = 160) +
               s(ffdi_final, k = 80) +
               s(rh98, k = 80) +
               s(cover_z_1, k = 80) +
               s(over_cover) +
               s(fhd_normal, k = 80) +
               FireTypeCategoryId,
             data = train,
             family = binomial,
             method = "fREML")
  setwd("/glade/scratch/kjfuller/data/chapter3/GAMs")
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
}
gamfun(sev = 1)
gamfun(sev = 2)
gamfun(sev = 3)
gamfun(sev = 4)