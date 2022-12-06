library(sf)
library(raster)
library(tidyverse)
library(tmap)
library(mgcv)

setwd("E:/chapter3/GEDI_FESM")
smotefun = function(x){
  g = st_read(paste0("ch3_forGAMs_prefire", x, "_allvars.gpkg"))
  g = g |> 
    filter(!is.na(LFMC) & !is.na(stringybark) & !is.na(ribbonbark)) |> 
    filter(fire_reg != 7 & fire_reg != 9)
}


p = ggplot(g, aes(prog)) +
  geom_histogram(bins = 100)
g |> group_by(fire_reg) |> tally() |> as.data.frame()
st_geometry(g) = NULL
g$log_string = log(g$stringybark)
g$log_ribbon = log(g$ribbonbark)
g$severity = as.factor(g$severity)
levels(g$severity) = c(0:3)

set.seed(5)
gam1 = gam(list(severity ~
             s(slope) +
             s(northness) +
             # s(eastness) +
             # log_string +
             # log_ribbon +
             # s(LFMC, k = 160) +
             # s(VPD, k = 160) +
             # s(rh98, k = 40) +
             # s(cover_z_1) +
             # s(over_cover, k = 80) +
             s(fhd_normal), 
             ~ s(slope) +
               s(northness) +
               # s(eastness) +
               # log_string +
               # log_ribbon +
               # s(LFMC, k = 160) +
               # s(VPD, k = 160) +
               # s(rh98, k = 40) +
               # s(cover_z_1) +
               # s(over_cover, k = 80) +
               s(fhd_normal),
             ~ s(slope) +
               s(northness) +
               # s(eastness) +
               # log_string +
               # log_ribbon +
               # s(LFMC, k = 160) +
               # s(VPD, k = 160) +
               # s(rh98, k = 40) +
               # s(cover_z_1) +
               # s(over_cover, k = 80) +
               s(fhd_normal),
             ~ s(slope) +
               s(northness) +
               # s(eastness) +
               # log_string +
               # log_ribbon +
               # s(LFMC, k = 160) +
               # s(VPD, k = 160) +
               # s(rh98, k = 40) +
               # s(cover_z_1) +
               # s(over_cover, k = 80) +
               s(fhd_normal)),
           data = g,
           family = mgcv::multinom(K = 3))
# saveRDS(gam1, paste0(modelnom, ".rds"))
## Error in gam(list(severity ~ s(slope) + s(northness) + s(fhd_normal),  : 
# incorrect number of linear predictors for family


par(mfrow = c(2, 2))
gam.check(gam1)

anova(gam1)

summary(gam1)

plot(gam1, pages = 1,
     all.terms = T,
     rug = T,
     se = T,
     shade = T,
     shift = coef(gam1)[1])

cor()