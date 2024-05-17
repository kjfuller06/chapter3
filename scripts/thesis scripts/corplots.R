library(tidyverse)
library(sf)
library(corrplot)

setwd("E:/chapter3/for GAMs")
g = st_read("isochrons_prep5.gpkg")
st_geometry(g) = NULL

g = g %>%
  dplyr::select(area,
                fireline,
                progtime,
                poly_sD,
                poly_eD,
                prog,
                logprog,
                spread,
                logspread,
                fire_reg,
                rh98.all:Gmethod.all,
                rh98.12.5:fhd_normal.1000,
                meanffd:ffdi_final,
                ffdi_cat,
                rough.1:slope.9,
                # elev.1:elev.9,
                stringybark.1:ribbonbark.9,
                LFMC:winddir,
                windnorth,
                windeast,
                strahler4:strahler11,
                s4:s11,
                strahler4.only:strahler11.only,
                s4.only:s11.only)
g$stringybark.5 = as.numeric(g$stringybark.5)
g$ribbonbark.5 = as.numeric(g$ribbonbark.5)

# isochron characteristics
setwd("C:/chapter3/outputs")
jpeg("corr_isochronvars.jpeg", width = 1500, height = 1500, res = 300)
c1 = cor(g |> dplyr::select(area:progtime, prog, spread), method = "spearman")
c1 |> 
  corrplot(
    order="AOE", 
    type = "lower",
    diag = F,
    method = "circle", na.label = "NA",
    addCoef.col ='black',
    tl.col = "black")
dev.off()

# vegetation
setwd("C:/chapter3/outputs")
jpeg("corr_vegvars.jpeg", width = 1500, height = 1500, res = 300)
c1 = cor(g |> dplyr::select(rh98.all:fhd_normal.all, stringybark.5, ribbonbark.5), method = "spearman")
c1 |> 
  corrplot(
    order="AOE", 
    type = "lower",
    diag = F,
    method = "circle", na.label = "NA",
    addCoef.col ='black',
    tl.col = "black")
dev.off()

g.temp = na.omit(g)
jpeg("corr_vegvars_na.omit.jpeg", width = 4500, height = 4500, res = 300)
c1 = cor(g.temp |> dplyr::select(rh98.12.5, rh98.25, rh98.100, rh98.1000, rh98.all, cover.12.5, cover.25, cover.100, cover.1000, cover.all, cover_z_1.12.5, cover_z_1.25, cover_z_1.100, cover_z_1.1000, cover_z_1.all, over_cover.12.5, over_cover.25, over_cover.100, over_cover.1000, over_cover.all, fhd_normal.12.5, fhd_normal.25, fhd_normal.100, fhd_normal.1000, fhd_normal.all), method = "spearman")
c1 |> 
  corrplot(
    order="original", 
    type = "lower",
    diag = F,
    method = "circle", na.label = "NA",
    addCoef.col ='black',
    tl.col = "black")
dev.off()
## all strongly correlated (> 0.7)

# weather
setwd("C:/chapter3/outputs")
jpeg("corr_weathervars.jpeg", width = 1500, height = 1500, res = 300)
c1 = cor(g |> dplyr::select(ffdi_final, LFMC, VPD, windspeed, windgust, winddir, windeast, windnorth), method = "spearman")
c1 |> 
  corrplot(
    order="AOE", 
    type = "lower",
    diag = F,
    method = "circle", na.label = "NA",
    addCoef.col ='black',
    tl.col = "black")
dev.off()

# terrain
setwd("C:/chapter3/outputs")
jpeg("corr_terrainvars.jpeg", width = 3000, height = 3000, res = 300)
c1 = cor(g |> dplyr::select(slope.5, rough.5, s4:s11, s4.only:s11.only), method = "spearman")
c1 |> 
  corrplot(
    order="AOE", 
    type = "lower",
    diag = F,
    method = "circle", na.label = "NA",
    addCoef.col ='black',
    tl.col = "black")
dev.off()

# all characteristics
setwd("C:/chapter3/outputs")
jpeg("corr_allvars.jpeg", width = 6000, height = 6000, res = 300)
c1 = cor(g |> dplyr::select(area:progtime, prog, spread, rh98.dist:fhd_normal.dist, stringybark.5, ribbonbark.5, ffdi_final, LFMC, VPD, windspeed, windgust, winddir, windeast, windnorth, rough.5, s4:s11, s4.only:s11.only), method = "spearman")
c1 |> 
  corrplot(
    order="original", 
    type = "lower",
    diag = F,
    method = "circle", na.label = "NA",
    addCoef.col ='black',
    tl.col = "black")
dev.off()
