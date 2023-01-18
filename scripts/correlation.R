library(sf)
library(tidyverse)
library(psych)
library(corrplot)
library(raster)
library(exactextractr)
library(tmap)

# cor plots ####
setwd("E:/chapter3/for GAMs")
dat = st_read("ch3_forGAMs_poly_prefire180_final_redo.gpkg")
st_geometry(dat) = NULL

dat$breaks = apply(dat |> dplyr::select(firelines, roads), 1, FUN = min, na.rm = T)

dat2 = dat |> dplyr::select(elevation_sd, water2, water3, water4, breaks, windspeed, windspeed.1, windspeed.9, windspeed.stdev, windgust, windgust.9, windgust.stdev, winddir.stdev, VPD, VPD.1, VPD.9, LFMC, LFMC.1, LFMC.9, rh98:fhd_normal, stringybark, ribbonbark)
names(dat2) = c("Standard devation in elevation",
             "Median distance to top two\norder water features",
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
             "Median cover",
             "Median understorey cover",
             "Median overstorey cover",
             "Median foliage height diversity index",
             "Median stringbark probability",
             "Median ribboning proabability")
setwd("D:/chapter3/outputs")
jpeg("corr_all.jpeg", width = 5000, height = 5000, res = 300)
cor(dat2, method = "spearman") |> 
  corrplot(
    order="original",
    type = "lower", 
    method = "circle", diag = F, 
    addCoef.col ='black',
    tl.col = "black",
    tl.srt = 45)
dev.off()

dat2 = dat |> dplyr::select(elevation_sd, water2,breaks, windspeed, windspeed.stdev, windgust, windgust.stdev, winddir.stdev, VPD, LFMC, rh98, cover_z_1, over_cover, fhd_normal, stringybark, ribbonbark)
names(dat2) = c("Standard devation in elevation",
                "Median distance to top two\norder water features",
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
jpeg("corr_target.jpeg", width = 3000, height = 3000, res = 300)
cor(dat2, method = "spearman") |> 
  corrplot(
    order="original",
    type = "lower", 
    method = "circle", diag = F, 
    addCoef.col ='black',
    tl.col = "black",
    tl.srt = 30,
    number.cex = 0.5)
dev.off()

jpeg("corr_fuel.jpeg", width = 2000, height = 2000, res = 250)
cor(dat2 |> dplyr::select(11:15), method = "spearman") |> 
  corrplot(
    order="FPC",
    type = "lower", 
    method = "circle", diag = F, 
    addCoef.col ='black',
    tl.col = "black",
    tl.srt = 45)
dev.off()

# understorey cover versus precip ####
setwd("D:/chapter1/other_data/Original/aridity/ai_et0")
list.files()
r = raster("ai_et0.tif")
setwd("D:/chapter1/bark-type-SDM/data")
nsw = st_read("NSW_sans_islands.shp") |> 
  st_transform(crs = st_crs(r))
r_crop = crop(r, nsw)
tm_shape(r_crop) + tm_raster()

setwd("E:/chapter3/for GAMs")
g = st_read("ch3_forGAMs_poly_prefire180_final_redo.gpkg")
targetcrs = st_crs(g)
g$logprog = log(g$prog)
g = st_transform(g, crs = st_crs(r))

l = raster::extract(r, g, method = 'simple')
g$aridity = unlist(lapply(l, FUN = function(x) median(x)))
summary(g)

g = g |> 
  filter(!is.na(aridity))
g = st_transform(g, crs = targetcrs)
st_write(g, "check_aridity.gpkg", delete_dsn = T)
g = st_read("check_aridity.gpkg")
st_geometry(g) = NULL

cor(g |> dplyr::select(rh98:fhd_normal, stringybark, aridity, logprog), method = "spearman")

ggplot(g, aes(x = aridity, y = cover_z_1)) + geom_point() + geom_smooth(method = "lm")
ggplot(g, aes(x = aridity, y = logprog)) + geom_point() + geom_smooth(method = "lm")
ggplot(g, aes(x = over_cover, y = cover_z_1)) + geom_point() + geom_smooth(method = "lm")
ggplot(g, aes(x = poly_sD, y = cover_z_1)) + geom_point() + geom_smooth(method = "lm")
ggplot(g, aes(x = poly_sD, y = logprog)) + geom_point() + geom_smooth(method = "lm")

gedi = st_read(paste0("ch3_isochrons_prefire180.gpkg"))
targetcrs = st_crs(gedi)
st_geometry(gedi) = NULL

# alternative gam ####
g = st_read(paste0("ch3_forGAMs_poly_prefire", x, "_final_redo.gpkg"))
g$fire_reg = as.factor(g$fire_reg)
targetcrs = st_crs(g)
st_geometry(g) = NULL

g = na.omit(g)
g$ffdi_cat = factor(g$ffdi_cat, levels = c("one",
                                           "two",
                                           "three",
                                           "four"))

sb = g %>%
  initial_split(strata = prog, prop = 7/10, seed = 10)
test = testing(sb)
train = training(sb)
train$fire_reg = as.factor(train$fire_reg)

g = st_read("check_aridity.gpkg")
st_geometry(g) = NULL
g$fire_reg = as.factor(g$fire_reg)
train = full_join(train, g)
train = filter(train, !is.na(aridity))
g = train

g$logprog = log(g$prog)

set.seed(5)
gam1 = bam(logprog ~ 
             # fire_reg +
             # s(rh98, by = ffdi_cat) +
             # s(cover_z_1, by = ffdi_cat) +
             # s(over_cover, by = ffdi_cat) +
             # s(fhd_normal, by = ffdi_cat) +
             # s(rh98) + ## 59.7
             # s(over_cover) + ## 59.7
             s(cover_z_1, k = 3) + ## 60.1
             # s(cover_ratio) +
             # s(fhd_normal) + ## 59.4
             # s(maxtemp) +
             # s(maxrh) +
             # s(maxws) +
             # s(ffdi_final, k = 20) + ns
             # ffdi_cat +
             s(elevation_sd, k = 200) +
             s(stringybark) + ## 60.3
             # s(stringybark.1, k = 320) + ## 60.2
             # s(stringybark.9, k = 320) + ## 59.9
             # log_string +
             # s(ribbonbark) + ## 60.7
             # s(ribbonbark.1) + ## 61
             # s(ribbonbark.9) + ## 60.9 ns
             # log_ribbon + ## 60.3
             s(LFMC.1, k = 80) + ## 60.1
             # s(LFMC.9) + ## 58.7
             # s(LFMC) + ## 58.5
             # s(VPD.9) + ## 60.1
             # s(VPD.1) + ## 60.5
             s(VPD, k = 342) + ## 60.3 ## relationship looks better
             # s(winddiff.iso) +
             # s(windspeed) + ## 60.2
             # s(windspeed.1, VPD) + ## 60.5
             # s(windspeed.9) + ## 60.1
             s(wind.stdev, k = 15) +
             s(windspeed.stdev, k = 15) +
             # s(windspeed.stdev, wind.stdev, k = 15) +
             # s(windgust.stdev, k = 15) +
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
aic1 = AIC(logLik.gam(gam1))

set.seed(5)
gam2 = bam(logprog ~ 
             # fire_reg +
             # s(rh98, by = ffdi_cat) +
             # s(cover_z_1, by = ffdi_cat) +
             # s(over_cover, by = ffdi_cat) +
             # s(fhd_normal, by = ffdi_cat) +
             # s(rh98) + ## 59.7
             # s(over_cover) + ## 59.7
             # s(cover_z_1, k = 3) + ## 60.1
             s(aridity) +
             # s(cover_ratio) +
             # s(fhd_normal) + ## 59.4
             # s(maxtemp) +
             # s(maxrh) +
             # s(maxws) +
             # s(ffdi_final, k = 20) + ns
             # ffdi_cat +
             s(elevation_sd, k = 200) +
             s(stringybark) + ## 60.3
             # s(stringybark.1, k = 320) + ## 60.2
             # s(stringybark.9, k = 320) + ## 59.9
             # log_string +
             # s(ribbonbark) + ## 60.7
             # s(ribbonbark.1) + ## 61
             # s(ribbonbark.9) + ## 60.9 ns
             # log_ribbon + ## 60.3
             s(LFMC.1, k = 80) + ## 60.1
             # s(LFMC.9) + ## 58.7
             # s(LFMC) + ## 58.5
             # s(VPD.9) + ## 60.1
             # s(VPD.1) + ## 60.5
             s(VPD, k = 342) + ## 60.3 ## relationship looks better
             # s(winddiff.iso) +
             # s(windspeed) + ## 60.2
             # s(windspeed.1, VPD) + ## 60.5
             # s(windspeed.9) + ## 60.1
             s(wind.stdev, k = 15) +
             s(windspeed.stdev, k = 15) +
             # s(windspeed.stdev, wind.stdev, k = 15) +
             # s(windgust.stdev, k = 15) +
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
aic2 = AIC(logLik.gam(gam2))

aic1
aic2
summary(gam1)
summary(gam2)
anova(gam1)
anova(gam2)

plot(gam1, pages = 1,
     all.terms = T,
     rug = T,
     se = T,
     shade = T,
     shift = coef(gam1)[1])
plot(gam2, pages = 1,
     all.terms = T,
     rug = T,
     se = T,
     shade = T,
     shift = coef(gam2)[1])

par(mfrow = c(1, 2))
p1 = plot_smooth(gam1, view = "cover_z_1", n.grid = 1000)
p1 = plot_smooth(gam2, view = "aridity", n.grid = 1000)

par(mfrow = c(2, 2))
gam.check(gam1)
k.check(gam1)

cor(g |> dplyr::select(stringybark, aridity), method = "spearman")

g1 = st_read("check_pet.gpkg")
st_geometry(g1) = NULL
g1$fire_reg = as.factor(g1$fire_reg)
g = full_join(g, g1)

g1 = st_read("check_precip.gpkg")
st_geometry(g1) = NULL
g1$fire_reg = as.factor(g1$fire_reg)
g = full_join(g, g1)
g = na.omit(g)

cor(g |> dplyr::select(stringybark, cover_z_1, aridity, pet, precip, LFMC.1, VPD), method = "spearman")
