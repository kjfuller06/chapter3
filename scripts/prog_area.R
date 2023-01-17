# start ####
library(sf)
library(tidyverse)
# library(UBL)
library(gam)
library(mgcv)
library(ggpubr)
library(tidymodels)
library(patchwork)
library(viridis)
library(RColorBrewer)
# library(cowplot)
library(itsadug)
# library(mgcViz)
library(metR)
# library(ggh4x)
library(tmap)
library(raster)

x = 180
modelnom = paste0("ch3_GAM_iso_prefire", x)

setwd("E:/chapter3/for GAMs")
# g = st_read(paste0("ch3_forGAMs_poly_prefire", x, "_final.gpkg"))
g = st_read(paste0("ch3_forGAMs_poly_prefire", x, "_final_redo.gpkg"))
names(g)
g$area = as.numeric(st_area(g)/1000000)
summary(g)
st_geometry(g) = NULL

with(g, plot(area ~ prog))
lm1 = lm(data = g, area ~ prog)
summary(lm1)
cor(g |> dplyr::select(prog, area), method = "spearman")

g1 = ggplot(g, aes(x = prog, y = area)) +
  geom_point() +
  geom_smooth(method = "lm", col = "black") +
  xlab(expression("Fire progression rate (km"^2*"/hr)")) +
  ylab(expression("Area burned (km"^2*")")) +
  ggtitle("Spearman correlation coefficient = 0.95") +
  theme_bw()
g1
