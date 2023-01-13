library(sf)
library(tidyverse)
library(psych)
library(corrplot)

setwd("E:/chapter3/for GAMs")
g = st_read("ch3_forGAMs_poly_prefire180_final.gpkg")
st_geometry(g) = NULL

g = g |> dplyr::select(elevation_sd, water2, windspeed, windgust, wind.stdev, VPD, LFMC, rh98:fhd_normal, stringybark, ribbonbark)
names(g) = c("Standard devation in elevation",
             "Median distance to water",
             "Median wind speed",
             "Median wind gust",
             "Standard deviation in wind direction",
             "Median vapour pressure deficit",
             "Median live fuel moisture content",
             "Median maximum canopy height",
             "Median understorey cover",
             "Median overstorey cover",
             "Median foliage height diversity index",
             "Median stringbark probability",
             "Median ribboning proabability")

setwd("D:/chapter3/outputs")
jpeg("corr.jpeg", width = 3000, height = 3000, res = 300)
cor(g, method = "spearman") |> 
  corrplot(
    order="original",
    type = "lower", 
    method = "circle", diag = F, 
    addCoef.col ='black',
    tl.col = "black",
    tl.srt = 45)
dev.off()

jpeg("corr_fuel.jpeg", width = 1000, height = 1000, res = 250)
cor(g |> dplyr::select(rh98:fhd_normal), method = "spearman") |> 
  corrplot(
    order="FPC",
    type = "lower", 
    method = "circle", diag = F, 
    addCoef.col ='black',
    tl.col = "black",
    tl.srt = 45)
dev.off()



