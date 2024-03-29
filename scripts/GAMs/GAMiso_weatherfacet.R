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
# g = read.csv(paste0("ch3_forGAMs_poly_prefire", x, "_smote.csv"))
g = read.csv(paste0("trainingdata_prefire", x, "_iso.csv"))
g$ffdi_cat = as.factor(g$ffdi_cat)
g$fire_reg = as.factor(g$fire_reg)

g$logprog = log(g$prog)

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

# train = train |> 
#   filter(VPD < 8) |> 
#   filter(wind.stdev < 150) |> 
#   filter(elevation_sd < 300)

set.seed(100)
gam1 = bam(logprog ~ 
             # fire_reg +
             # s(rh98, by = ffdi_cat) +
             # s(cover_z_1, by = ffdi_cat) +
             # s(over_cover, by = ffdi_cat) +
             # s(fhd_normal, by = ffdi_cat) +
             # s(rh98) + ## 59.7
             # s(over_cover) + ## 59.7
             s(cover_z_1, LFMC.1) + ## 60.1
             # s(cover_ratio) +
             # s(fhd_normal) + ## 59.4
             # s(maxtemp) +
             # s(maxrh) +
             # s(maxws) +
             # s(ffdi_final, k = 20) + ns
             # ffdi_cat +
             s(elevation_sd, k = 150) +
             s(stringybark) + ## 60.3
             # s(stringybark.1, k = 320) + ## 60.2
             # s(stringybark.9, k = 320) + ## 59.9
             # log_string +
             # s(ribbonbark) + ## 60.7
             # s(ribbonbark.1) + ## 61
             # s(ribbonbark.9) + ## 60.9 ns
             # log_ribbon + ## 60.3
             # s(LFMC.1, k = 80) + ## 60.1
             # s(LFMC.9) + ## 58.7
             # s(LFMC) + ## 58.5
             # s(VPD.9) + ## 60.1
             # s(VPD.1) + ## 60.5
             s(VPD, k = 342) + ## 60.3 ## relationship looks better
             # s(winddiff.iso) +
             # s(windspeed) + ## 60.2
             # s(windspeed.1) + ## 60.5
             # s(windspeed.9) + ## 60.1
             s(wind.stdev, k = 15) +
             # s(windgust) + ## 60.3
             # s(windgust.9) + ## 60.3 ns
             # s(windgust, stringybark) +
             # s(windgust, ribbonbark.9) +
             # s(breaks) +
             # s(breaks.all4),
             # s(house.density) +
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

gam1 = readRDS(paste0(modelnom, ".rds"))

setwd("D:/chapter3/outputs/GAMs")
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

jpeg(paste0(modelnom, "_summedeffectplots.jpeg"), width = 3000, height = 2000, res = 200, units = "px")
par(mfrow = c(3, 3))
p1 = plot_smooth(gam1, view = "cover_z_1", n.grid = 1000)
p1 = plot_smooth(gam1, view = "elevation_sd", n.grid = 1000)
p1 = plot_smooth(gam1, view = "stringybark", n.grid = 1000)
# p1 = plot_smooth(gam1, view = "ribbonbark.9", n.grid = 1000)
p1 = plot_smooth(gam1, view = "LFMC.1", n.grid = 1000)
p1 = plot_smooth(gam1, view = "VPD", n.grid = 1000)
p1 = plot_smooth(gam1, view = "wind.stdev", n.grid = 1000)
p1 = plot_smooth(gam1, view = "water2", n.grid = 1000)
dev.off()

vals = data.frame(variable = c("cover_z_1",
                               "elevation_sd",
                               "stringybark",
                               # "ribbonbark.9",
                               "LFMC.1",
                               "VPD",
                               "wind.stdev",
                               "water2"),
                  median = c(0.39121350646019,
                             39.4739008012175,
                             0.673583328723907,
                             # 0.740256667137146,
                             67.8054453001101,
                             3.24551010131836,
                             45.1852170633562,
                             8612.2001953125))
write.csv(vals, paste0(modelnom, "_medianvaluesforplotting.csv"), row.names = F)

saveRDS(gam1, paste0(modelnom, ".rds"))
# gam1 = readRDS(paste0(modelnom, ".rds"))

setwd("E:/chapter3/for GAMs")
test = read.csv(paste0("testingdata_prefire", x, "_iso.csv"))
test$logprog = log(test$prog)

preds<-predict(gam1, type="response", newdata=test,
               se.fit=TRUE)

test$prediction = preds$fit
test$pred.se = preds$se.fit

test$error = test$prediction - test$logprog
R2 = 1 - (sum((test$logprog - test$prediction)^2)/sum((test$logprog - mean(test$logprog))^2))

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

calcfun = function(testx, predx){
  x = test.temp[,testx]
  preds.temp = rowSums(preds$fit) + coef(gam1)[1]
  fit = data.frame(fit = preds.temp*100, x = x)
  fit$ll = fit$fit - 196*preds$se.fit[,predx]
  fit$ll = gam1$family$linkinv(fit$ll)
  fit$ul = fit$fit + 196*preds$se.fit[,predx]
  fit$ul = gam1$family$linkinv(fit$ul)
  fit$ymax = max(c(fit$ul), na.rm = T) + 0.1
  fit$ymin = min(c(fit$ll), na.rm = T) - 0.1
  return(fit)
} 


test.temp = expand.grid(cover_z_1 = seq(from = min(g$cover_z_1), to = max(g$cover_z_1), length.out = 100),
                        LFMC.1 = seq(from = min(g$LFMC.1), to = max(g$LFMC.1), length.out = 100),
                        elevation_sd = median(g$elevation_sd),
                        stringybark = median(g$stringybark),
                        VPD = median(g$VPD),
                        wind.stdev = median(g$wind.stdev),
                        water2 = median(g$water2))

preds<-predict(gam1, type="terms", newdata=test.temp,
               se.fit=TRUE)

preds.temp = rowSums(preds$fit) + coef(gam1)[1]
fit = data.frame(fit = preds.temp, x = test.temp$cover_z_1, y = test.temp$LFMC.1)
fit$se = preds$se.fit[,1]
fit$ll = fit$fit - 1.96*preds$se.fit[,1]
fit$ll = gam1$family$linkinv(fit$ll)
fit$ul = fit$fit + 1.96*preds$se.fit[,1]
fit$ul = gam1$family$linkinv(fit$ul)
fit$ymax = max(c(fit$ul), na.rm = T) + 0.1
fit$ymin = min(c(fit$ll), na.rm = T) - 0.1

contour1 = ggplot(fit, aes(x=x, y=y)) + 
  geom_tile(aes(fill = fit)) +
  scale_fill_gradient(low = "grey70", high = "darkgreen") +
  geom_contour(aes(z = fit), colour = "white", 
               # binwidth = binwidth
               ) +
  # geom_text_contour(aes(z = fit), skip = 0, 
  #                   # breaks = breaks, 
  #                   colour = "white") +
  xlab("cover_z_1") +
  ylab("LFMC") +
  theme_bw()

contour2 = ggplot(fit, aes(x=x, y=y)) + 
  geom_tile(aes(fill = se)) +
  scale_fill_gradient(low = "grey70", high = "darkgreen") +
  geom_contour(aes(z = se), colour = "white", 
               # binwidth = binwidth
  ) +
  # geom_text_contour(aes(z = se), skip = 0, 
  #                   # breaks = breaks, 
  #                   colour = "white") +
  xlab("cover_z_1") +
  ylab("LFMC") +
  theme_bw()

contour1|contour2

## one at a time
ymin = -5
ymax = 2
test.temp = expand.grid(cover_z_1 = seq(from = min(g$cover_z_1), to = max(g$cover_z_1), length.out = 100),
                        LFMC.1 = quantile(g$LFMC.1, probs = 0),
                        elevation_sd = median(g$elevation_sd),
                        stringybark = median(g$stringybark),
                        VPD = median(g$VPD),
                        wind.stdev = median(g$wind.stdev),
                        water2 = median(g$water2))

preds<-predict(gam1, type="terms", newdata=test.temp,
               se.fit=TRUE)
fit = data.frame(fit = preds$fit[,1], x = test.temp$cover_z_1, y = test.temp$LFMC.1)
fit$se = preds$se.fit[,1]
fit$ll = fit$fit - 1.96*preds$se.fit[,1]
fit$ll = gam1$family$linkinv(fit$ll)
fit$ul = fit$fit + 1.96*preds$se.fit[,1]
fit$ul = gam1$family$linkinv(fit$ul)

p1 = ggplot(fit, aes(x = x, y = fit)) +
  geom_line(color = "black") +
  geom_ribbon(aes(ymin=ll, ymax=ul), alpha=0.15, fill = "black") +
  # geom_rug(data = rugdat, aes(x = predictor, y = response), sides = "b") +
  geom_abline(slope = 0, intercept = 0, lty = 2) +
  geom_abline(slope = 0, intercept = max(fit$fit), lty = 2) +
  coord_cartesian(ylim = c(ymin, ymax)) +
  xlab("cover_z_1") +
  ylab("Fire progression rate") +
  theme_bw()

test.temp = expand.grid(cover_z_1 = seq(from = min(g$cover_z_1), to = max(g$cover_z_1), length.out = 100),
                        LFMC.1 = quantile(g$LFMC.1, probs = 0.25),
                        elevation_sd = median(g$elevation_sd),
                        stringybark = median(g$stringybark),
                        VPD = median(g$VPD),
                        wind.stdev = median(g$wind.stdev),
                        water2 = median(g$water2))

preds<-predict(gam1, type="terms", newdata=test.temp,
               se.fit=TRUE)
fit = data.frame(fit = preds$fit[,1], x = test.temp$cover_z_1, y = test.temp$LFMC.1)
fit$se = preds$se.fit[,1]
fit$ll = fit$fit - 1.96*preds$se.fit[,1]
fit$ll = gam1$family$linkinv(fit$ll)
fit$ul = fit$fit + 1.96*preds$se.fit[,1]
fit$ul = gam1$family$linkinv(fit$ul)

p2 = ggplot(fit, aes(x = x, y = fit)) +
  geom_line(color = "black") +
  geom_ribbon(aes(ymin=ll, ymax=ul), alpha=0.15, fill = "black") +
  # geom_rug(data = rugdat, aes(x = predictor, y = response), sides = "b") +
  geom_abline(slope = 0, intercept = 0, lty = 2) +
  geom_abline(slope = 0, intercept = max(fit$fit), lty = 2) +
  coord_cartesian(ylim = c(ymin, ymax)) +
  xlab("cover_z_1") +
  ylab("Fire progression rate") +
  theme_bw()

test.temp = expand.grid(cover_z_1 = seq(from = min(g$cover_z_1), to = max(g$cover_z_1), length.out = 100),
                        LFMC.1 = quantile(g$LFMC.1, probs = 0.5),
                        elevation_sd = median(g$elevation_sd),
                        stringybark = median(g$stringybark),
                        VPD = median(g$VPD),
                        wind.stdev = median(g$wind.stdev),
                        water2 = median(g$water2))

preds<-predict(gam1, type="terms", newdata=test.temp,
               se.fit=TRUE)
fit = data.frame(fit = preds$fit[,1], x = test.temp$cover_z_1, y = test.temp$LFMC.1)
fit$se = preds$se.fit[,1]
fit$ll = fit$fit - 1.96*preds$se.fit[,1]
fit$ll = gam1$family$linkinv(fit$ll)
fit$ul = fit$fit + 1.96*preds$se.fit[,1]
fit$ul = gam1$family$linkinv(fit$ul)

p3 = ggplot(fit, aes(x = x, y = fit)) +
  geom_line(color = "black") +
  geom_ribbon(aes(ymin=ll, ymax=ul), alpha=0.15, fill = "black") +
  # geom_rug(data = rugdat, aes(x = predictor, y = response), sides = "b") +
  geom_abline(slope = 0, intercept = 0, lty = 2) +
  geom_abline(slope = 0, intercept = max(fit$fit), lty = 2) +
  coord_cartesian(ylim = c(ymin, ymax)) +
  xlab("cover_z_1") +
  ylab("Fire progression rate") +
  theme_bw()

test.temp = expand.grid(cover_z_1 = seq(from = min(g$cover_z_1), to = max(g$cover_z_1), length.out = 100),
                        LFMC.1 = quantile(g$LFMC.1, probs = 0.75),
                        elevation_sd = median(g$elevation_sd),
                        stringybark = median(g$stringybark),
                        VPD = median(g$VPD),
                        wind.stdev = median(g$wind.stdev),
                        water2 = median(g$water2))

preds<-predict(gam1, type="terms", newdata=test.temp,
               se.fit=TRUE)
fit = data.frame(fit = preds$fit[,1], x = test.temp$cover_z_1, y = test.temp$LFMC.1)
fit$se = preds$se.fit[,1]
fit$ll = fit$fit - 1.96*preds$se.fit[,1]
fit$ll = gam1$family$linkinv(fit$ll)
fit$ul = fit$fit + 1.96*preds$se.fit[,1]
fit$ul = gam1$family$linkinv(fit$ul)

p4 = ggplot(fit, aes(x = x, y = fit)) +
  geom_line(color = "black") +
  geom_ribbon(aes(ymin=ll, ymax=ul), alpha=0.15, fill = "black") +
  # geom_rug(data = rugdat, aes(x = predictor, y = response), sides = "b") +
  geom_abline(slope = 0, intercept = 0, lty = 2) +
  geom_abline(slope = 0, intercept = max(fit$fit), lty = 2) +
  coord_cartesian(ylim = c(ymin, ymax)) +
  xlab("cover_z_1") +
  ylab("Fire progression rate") +
  theme_bw()

test.temp = expand.grid(cover_z_1 = seq(from = min(g$cover_z_1), to = max(g$cover_z_1), length.out = 100),
                        LFMC.1 = quantile(g$LFMC.1, probs = 1),
                        elevation_sd = median(g$elevation_sd),
                        stringybark = median(g$stringybark),
                        VPD = median(g$VPD),
                        wind.stdev = median(g$wind.stdev),
                        water2 = median(g$water2))

preds<-predict(gam1, type="terms", newdata=test.temp,
               se.fit=TRUE)
fit = data.frame(fit = preds$fit[,1], x = test.temp$cover_z_1, y = test.temp$LFMC.1)
fit$se = preds$se.fit[,1]
fit$ll = fit$fit - 1.96*preds$se.fit[,1]
fit$ll = gam1$family$linkinv(fit$ll)
fit$ul = fit$fit + 1.96*preds$se.fit[,1]
fit$ul = gam1$family$linkinv(fit$ul)

p5 = ggplot(fit, aes(x = x, y = fit)) +
  geom_line(color = "black") +
  geom_ribbon(aes(ymin=ll, ymax=ul), alpha=0.15, fill = "black") +
  # geom_rug(data = rugdat, aes(x = predictor, y = response), sides = "b") +
  geom_abline(slope = 0, intercept = 0, lty = 2) +
  geom_abline(slope = 0, intercept = max(fit$fit), lty = 2) +
  coord_cartesian(ylim = c(ymin, ymax)) +
  xlab("cover_z_1") +
  ylab("Fire progression rate") +
  theme_bw()

p1|p2|p3|p4|p5

##
fvisgam(gam1, view = c("cover_z_1", "LFMC.1"), se = 3, too.far = 0.1)
