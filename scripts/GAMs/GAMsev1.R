library(sf)
library(raster)
library(tidyverse)
library(tidymodels)
library(mgcv)
library(mgcViz)
library(DHARMa)

x = 180
# severity == 1 ####
sev = 1
modelnom = paste0("ch3_GAM_severity", sev, "_prefire", x)

setwd("E:/chapter3/for GAMs")
g = st_read(paste0("ch3_forGAMs_prefire", x, "_smotesev", sev, ".gpkg"))
## 16.8%, Radj 0.21
# g = st_read(paste0("trainingdata_prefire", x, "_sev", sev, ".gpkg"))
## 12.5%, Radj 0.135

st_geometry(g) = NULL
g$log_string = log(g$stringybark + 1)
g$log_ribbon = log(g$ribbonbark + 1)
g$severity = as.factor(g$severity)
g$category = as.factor(g$category)
g$fire_reg = as.factor(g$fire_reg)

set.seed(5)
gam1 = bam(severity ~ 
             # s(rh98) +
             s(cover_z_1) +
             # s(over_cover) +
             s(fhd_normal) +
             s(slope) +
             s(stringybark) +
             s(ribbonbark) +
             s(LFMC) +
             s(VPD) +
             s(winddiff.bom) +
             # s(windspeed) +
             # s(windspeed.9) +
             s(windgust) +
             # s(windgust.9) +
             # s(breaks) +
             s(breaks.all) +
             # s(water3) +
             # s(water4) +
             category,
           data = g, 
           family = binomial(link = "logit"),
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
dev.off()
k.check(gam1)
simulationOutput <- simulateResiduals(fittedModel = gam1)
# plot(simulationOutput, asFactor = T)
plotResiduals(simulationOutput, train$fire_reg, quantreg = T)

setwd("D:/chapter3/outputs/GAMs")
capture.output(
  paste0("AIC = ", AICc(gam1)),
  file = paste0(modelnom, "modeloutputs.txt"))
capture.output(
  print("**************************summary****************************"), summary(gam1),
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
p1 = plot_smooth(gam1, view = "winddiff.iso", n.grid = 1000)
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

# severity == 2 ####
sev = 2
modelnom = paste0("ch3_GAM_severity", sev, "_prefire", x)

setwd("E:/chapter3/for GAMs")
g = st_read(paste0("ch3_forGAMs_prefire", x, "_smotesev", sev, ".gpkg"))
## 6.79%; Radj 0.09
# g = st_read(paste0("trainingdata_prefire", x, "_sev", sev, ".gpkg"))
## 5.04%; Radj 0.06

st_geometry(g) = NULL
g$log_string = log(g$stringybark + 1)
g$log_ribbon = log(g$ribbonbark + 1)
g$severity = as.factor(g$severity)
g$category = as.factor(g$category)
g$fire_reg = as.factor(g$fire_reg)

set.seed(5)
gam1 = bam(severity ~ 
             s(rh98) +
             s(cover_z_1) +
             s(over_cover) +
             s(fhd_normal) +
             s(slope) +
             s(stringybark) +
             s(ribbonbark) +
             s(LFMC) +
             s(VPD) +
             s(winddiff.bom) +
             # s(windspeed) +
             # s(windspeed.9) +
             s(windgust) +
             # s(windgust.9) +
             # s(breaks) +
             s(breaks.all) +
             # s(water3) +
             # s(water4) +
             category,
           data = g, 
           family = binomial(link = "logit"),
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
dev.off()
k.check(gam1)
simulationOutput <- simulateResiduals(fittedModel = gam1)
# plot(simulationOutput, asFactor = T)
plotResiduals(simulationOutput, train$fire_reg, quantreg = T)

setwd("D:/chapter3/outputs/GAMs")
capture.output(
  paste0("AIC = ", AICc(gam1)),
  file = paste0(modelnom, "modeloutputs.txt"))
capture.output(
  print("**************************summary****************************"), summary(gam1),
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
p1 = plot_smooth(gam1, view = "winddiff.iso", n.grid = 1000)
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

# severity == 3 ####
sev = 3
modelnom = paste0("ch3_GAM_severity", sev, "_prefire", x)

setwd("E:/chapter3/for GAMs")
g = st_read(paste0("ch3_forGAMs_prefire", x, "_smotesev", sev, ".gpkg"))
# g = st_read(paste0("trainingdata_prefire", x, "_sev", sev, ".gpkg"))

st_geometry(g) = NULL
g$log_string = log(g$stringybark + 1)
g$log_ribbon = log(g$ribbonbark + 1)
g$severity = as.factor(g$severity)
g$category = as.factor(g$category)
g$fire_reg = as.factor(g$fire_reg)

set.seed(5)
gam1 = bam(severity ~ 
             # s(rh98) +
             s(cover_z_1) +
             # s(over_cover) +
             s(fhd_normal) +
             s(slope) +
             s(stringybark) +
             s(ribbonbark) +
             s(LFMC) +
             s(VPD) +
             s(winddiff.bom) +
             # s(windspeed) +
             # s(windspeed.9) +
             s(windgust) +
             # s(windgust.9) +
             # s(breaks) +
             s(breaks.all) +
             # s(water3) +
             # s(water4) +
             category,
           data = g, 
           family = binomial(link = "logit"),
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
dev.off()
k.check(gam1)
simulationOutput <- simulateResiduals(fittedModel = gam1)
# plot(simulationOutput, asFactor = T)
plotResiduals(simulationOutput, train$fire_reg, quantreg = T)

setwd("D:/chapter3/outputs/GAMs")
capture.output(
  paste0("AIC = ", AICc(gam1)),
  file = paste0(modelnom, "modeloutputs.txt"))
capture.output(
  print("**************************summary****************************"), summary(gam1),
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
p1 = plot_smooth(gam1, view = "winddiff.iso", n.grid = 1000)
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

# severity == 4 ####
sev = 4
modelnom = paste0("ch3_GAM_severity", sev, "_prefire", x)

setwd("E:/chapter3/for GAMs")
g = st_read(paste0("ch3_forGAMs_prefire", x, "_smotesev", sev, ".gpkg"))
# g = st_read(paste0("trainingdata_prefire", x, "_sev", sev, ".gpkg"))

st_geometry(g) = NULL
g$log_string = log(g$stringybark + 1)
g$log_ribbon = log(g$ribbonbark + 1)
g$severity = as.factor(g$severity)
g$category = as.factor(g$category)
g$fire_reg = as.factor(g$fire_reg)

set.seed(5)
gam1 = bam(severity ~ 
             # s(rh98) +
             s(cover_z_1) +
             # s(over_cover) +
             s(fhd_normal) +
             s(slope) +
             s(stringybark) +
             s(ribbonbark) +
             s(LFMC) +
             s(VPD) +
             s(winddiff.bom) +
             # s(windspeed) +
             # s(windspeed.9) +
             s(windgust) +
             # s(windgust.9) +
             # s(breaks) +
             s(breaks.all) +
             # s(water3) +
             # s(water4) +
             category,
           data = g, 
           family = binomial(link = "logit"),
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
dev.off()
k.check(gam1)
simulationOutput <- simulateResiduals(fittedModel = gam1)
# plot(simulationOutput, asFactor = T)
plotResiduals(simulationOutput, train$fire_reg, quantreg = T)

setwd("D:/chapter3/outputs/GAMs")
capture.output(
  paste0("AIC = ", AICc(gam1)),
  file = paste0(modelnom, "modeloutputs.txt"))
capture.output(
  print("**************************summary****************************"), summary(gam1),
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
p1 = plot_smooth(gam1, view = "winddiff.iso", n.grid = 1000)
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
