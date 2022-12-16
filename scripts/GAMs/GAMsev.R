library(sf)
library(raster)
library(tidyverse)
library(tidymodels)
library(mgcv)

x = 180
# severity == 1 ####
sev = 1
modelnom = paste0("ch3_GAM_severity", sev, "_prefire", x)

setwd("E:/chapter3/for GAMs")
g = st_read(paste0("ch3_forGAMs_prefire", x, "_smotesev", sev, ".gpkg"))
st_geometry(g) = NULL
g$log_string = log(g$stringybark)
g$log_ribbon = log(g$ribbonbark)
g$severity = as.factor(g$severity)
g$FireTypeCategoryId = as.factor(g$FireTypeCategoryId)

p1 = plot_smooth(gam1, view = "elevation", n.grid = 1000, cond=list(speifact='near normal'))

setwd("D:/chapter3/outputs/GAMs")
gam1 = readRDS(paste0(modelnom, ".rds"))

print("*******************  gam.check *********************")
jpeg(paste0(modelnom, "_diagnosticplots.jpeg"), width = 3000, height = 2000, res = 200, units = "px")
par(mfrow = c(2, 2))
gam.check(gam1)
dev.off()

