library(sf)
library(tidyverse)
library(psych)
library(corrplot)

setwd("E:/chapter3/for GAMs")
g = st_read("ch3_forGAMs_poly_prefire180_final_redo.gpkg")
st_geometry(g) = NULL

g$breaks = apply(g |> dplyr::select(firelines, roads), 1, FUN = min, na.rm = T)

g2 = g |> dplyr::select(elevation_sd, water2, water3, water4, breaks, windspeed, windspeed.1, windspeed.9, windspeed.stdev, windgust, windgust.9, windgust.stdev, wind.stdev, VPD, VPD.1, VPD.9, LFMC, LFMC.1, LFMC.9, rh98:fhd_normal, stringybark, ribbonbark)
names(g2) = c("Standard devation in elevation",
             "Median distance to first or second\norder water features",
             "Median distance to top three\norders of water features",
             "Median distance to top four\norders of water features",
             "Median distance to roads",
             "Median wind speed",
             "10th percentile of wind speed",
             "90th percentile of wind speed",
             "Standard deviation in wind speed",
             "Median wind gust",
             "90th percentile of wind gust",
             "Standard deviation in wind gust",
             "Standard deviation in wind direction",
             "Median vapour pressure deficit",
             "10th percentile of vapour pressure deficit",
             "90th percentile of vapour pressure deficit",
             "Median live fuel moisture content",
             "10th percentile of live fuel moisture content",
             "90th percentile of live fuel moisture content",
             "Median maximum canopy height",
             "Median understorey cover",
             "Median overstorey cover",
             "Median foliage height diversity index",
             "Median stringbark probability",
             "Median ribboning proabability")
setwd("D:/chapter3/outputs")
jpeg("corr_all.jpeg", width = 5000, height = 5000, res = 300)
cor(g2, method = "spearman") |> 
  corrplot(
    order="original",
    type = "lower", 
    method = "circle", diag = F, 
    addCoef.col ='black',
    tl.col = "black",
    tl.srt = 45)
dev.off()

g2 = g |> dplyr::select(elevation_sd, water2, water3, water4, breaks, windspeed, windspeed.stdev, windgust, windgust.stdev, wind.stdev, VPD, LFMC, rh98:fhd_normal, stringybark, ribbonbark)
names(g2) = c("Standard devation in elevation",
             "Median distance to first or second\norder water features",
             "Median distance to top three\norders of water features",
             "Median distance to top four\norders of water features",
             "Median distance to roads",
             "Median wind speed",
             "Standard deviation in wind speed",
             "Median wind gust",
             "Standard deviation in wind gust",
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
jpeg("corr_target.jpeg", width = 4000, height = 4000, res = 300)
cor(g2, method = "spearman") |> 
  corrplot(
    order="original",
    type = "lower", 
    method = "circle", diag = F, 
    addCoef.col ='black',
    tl.col = "black",
    tl.srt = 30)
dev.off()

g = g |> dplyr::select(elevation_sd, water2, breaks, windspeed, windgust, wind.stdev, VPD, LFMC, rh98:fhd_normal, stringybark, ribbonbark)
names(g) = c("Standard devation in elevation",
             "Median distance to first or second\norder water features",
             "Median distance to roads",
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
jpeg("corr_target.jpeg", width = 3000, height = 3000, res = 300)
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