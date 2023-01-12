library(sf)
library(raster)
library(tidyverse)
library(tidymodels)
library(mgcv)
library(mgcViz)
library(patchwork)
# library(DHARMa)

x = 180
modelnom = paste0("ch3_GAM_severity_prefire", x)

setwd("/glade/scratch/kjfuller/data/chapter3")
setwd("E:/chapter3/for GAMs")
g = st_read(paste0("ch3_forGAMs_prefire", x, "_smotesev.gpkg"))
## 16.8%, Radj 0.21
# g = st_read(paste0("trainingdata_prefire", x, "_sev.gpkg"))
## 12.5%, Radj 0.135

st_geometry(g) = NULL
g$log_string = log(g$stringybark + 1)
g$log_ribbon = log(g$ribbonbark + 1)
g$severity[g$severity == 2] = 0
g$severity[g$severity == 3] = 0
g$severity[g$severity == 4] = 1
g$severity[g$severity == 5] = 1
# g$severity = as.numeric(g$severity)
g$severity = as.factor(g$severity)
g$category = as.factor(g$category)
g$fire_reg = as.factor(g$fire_reg)

set.seed(5)
gam1 <- gamV(severity ~ 
               # fire_reg +
                    # s(rh98) +
                    s(cover_z_1, by = fire_reg) +
                    # s(over_cover) +
                    s(fhd_normal, by = fire_reg) +
                    s(slope) +
                    s(stringybark) +
                    s(ribbonbark) +
                    s(LFMC) +
                    s(VPD) +
                    # s(winddiff.bom) +
                    # s(windspeed) +
                    # s(windspeed.9) +
                    s(windgust) +
                    # s(windgust.9) +
                    # s(breaks) +
                    # s(breaks.all) +
                    # s(water3) +
                    # s(water4) +
                    category,
                  family = "binomial",
                  data = g |> filter(fire_reg != 8),
                  method = "REML")
summary(gam1)

plot(gam1, pages = 1,
     all.terms = T,
     rug = F,
     se = T,
     shade = T,
     shift = coef(gam1)[1])


set.seed(5)
# gam1 = gam(list(severity ~ 
#                   # s(rh98) +
#                   s(cover_z_1) +
#                   # s(over_cover) +
#                   s(fhd_normal) +
#                   s(slope) +
#                   s(stringybark) +
#                   s(ribbonbark) +
#                   s(LFMC) +
#                   s(VPD) +
#                   s(winddiff.bom) +
#                   # s(windspeed) +
#                   # s(windspeed.9) +
#                   s(windgust) +
#                   # s(windgust.9) +
#                   # s(breaks) +
#                   s(breaks.all) +
#                   # s(water3) +
#                   # s(water4) +
#                   category,
#                 ~ # s(rh98) +
#                   s(cover_z_1) +
#                   # s(over_cover) +
#                   s(fhd_normal) +
#                   s(slope) +
#                   s(stringybark) +
#                   s(ribbonbark) +
#                   s(LFMC) +
#                   s(VPD) +
#                   s(winddiff.bom) +
#                   # s(windspeed) +
#                   # s(windspeed.9) +
#                   s(windgust) +
#                   # s(windgust.9) +
#                   # s(breaks) +
#                   s(breaks.all) +
#                   # s(water3) +
#                   # s(water4) +
#                   category,
#                 ~ # s(rh98) +
#                   s(cover_z_1) +
#                   # s(over_cover) +
#                   s(fhd_normal) +
#                   s(slope) +
#                   s(stringybark) +
#                   s(ribbonbark) +
#                   s(LFMC) +
#                   s(VPD) +
#                   s(winddiff.bom) +
#                   # s(windspeed) +
#                   # s(windspeed.9) +
#                   s(windgust) +
#                   # s(windgust.9) +
#                   # s(breaks) +
#                   s(breaks.all) +
#                   # s(water3) +
#                   # s(water4) +
#                   category),
#            data = g, 
#            family = mgcv::multinom(K = 3),
#            method = "REML",
#            select = T)

gam1 <- gamV(list(severity ~ 
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
               ~ s(rh98) +
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
               ~ s(rh98) +
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
                 category),
          data = g,
          family=multinom(K=3),
          method = "REML"
          # ,
          # select = T
          )

saveRDS(gam1, paste0(modelnom, ".rds"))

setwd("D:/chapter3/outputs/GAMs")
gam1 = readRDS(paste0(modelnom, ".rds"))
print(plot(gam1), pages = 1)

predictors = names(gam1$model)[names(gam1$model) != "severity"]

for(i in c(1:length(predictors))){
  var = predictors[i]
  plots = list()
  for(i in c(1:4)){
    if(var == "category"){
      p_temp = ALE(gam1, x = var, oind = i, type = "response", center = 2)
      plots[[i]] = ggplot(data = p_temp$ALE$ALE, aes(x = x, y = y)) +
        geom_boxplot(aes(middle = y, ymin = y - se*1.96, lower = y - se*1.96, upper = y + se*1.96, ymax = y + se*1.96), fill = "grey70", stat= "identity")
    } else {
      p_temp = ALE(gam1, x = var, oind = i, type = "response", center = 2)
      plots[[i]] = ggplot(data = p_temp$ALE$ALE, aes(x = x, y = y)) +
        geom_ribbon(aes(ymin = y - se*1.96, ymax = y + se*1.96), fill = "grey70") + 
        geom_line()
    }
  }
  jpeg(paste0(modelnom, "_ALEplots_", var, ".jpeg"), width = 4000, height = 1000, res = 200, units = "px")
  p1 = plots[[1]] + ggtitle("Severity Low")
  p2 = plots[[2]] + ggtitle("Severity Moderate")
  p3 = plots[[3]] + ggtitle("Severity High")
  p4 = plots[[4]] + ggtitle("Severity Extreme")
  p1 | p2 | p3 | p4
  dev.off()
}