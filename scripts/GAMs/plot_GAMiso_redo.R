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
library(cowplot)
library(itsadug)
# library(mgcViz)
library(metR)
# library(ggh4x)
library(tmap)
library(raster)

x = 180
modelnom = paste0("ch3_GAM_iso_prefire", x, "_redo")

# plot functions ####
summedallfun = function(b1, config, ymin, ymax, dat, rugdat, lab2, ab2 = ab2){
    plot_temp = ggplot(dat, aes(x = x, y = fit)) +
      geom_line(color = "black") +
      geom_ribbon(aes(ymin=ll, ymax=ul), alpha=0.15, fill = "black") +
      # geom_rug(data = rugdat, aes(x = predictor, y = response), sides = "b") +
      geom_abline(slope = 0, intercept = 0, lty = 2) +
      geom_abline(slope = 0, intercept = ab2, lty = 2) +
      coord_cartesian(ylim = c(ymin, ymax))
    print("no groups")
  
  if(config == "xy"){
    plot_temp = plot_temp +
      geom_rug(data = rugdat, aes(x = predictor, y = logprog), sides = "b") +
      ylab(expression("Fire rate of spread (km"^2*"/hr)")) +
      xlab(lab2) +
      theme_bw() +
      theme(axis.text.y=element_text(size = 12),
            axis.text.x=element_text(size = 12)) +
      theme(plot.margin = unit(c(0.1,0.1,0,0), "cm"))
    print("config xy")
  } else if(config == "y"){
    plot_temp = plot_temp +
      ylab("Fire Rate of Spread (km^2/hr)") +
      xlab("") +
      theme_bw() +
      theme(axis.text.y=element_text(size = 12),
            axis.ticks.x=element_blank(),
            axis.text.x=element_blank(),
            axis.title.x = 
              element_text(margin = 
                             ggplot2::margin( 
                               b = -15))) +
      theme(plot.margin = unit(c(0.1,0.1,0,0), "cm"))
    print("config y")
  } else if(config == "x"){
    plot_temp = plot_temp +
      geom_rug(data = rugdat, aes(x = predictor, y = logprog), sides = "b") +
      ylab("") +
      xlab(lab2) +
      theme_bw() +
      theme(axis.ticks.y=element_blank(),
            axis.text.y=element_blank(),
            axis.text.x=element_text(size = 12),
            axis.title.y = 
              element_text(margin = 
                             ggplot2::margin( 
                               l = -15))) +
      theme(plot.margin = unit(c(0.1,0.1,0,0.1), "cm"))
    print("config x")
  } else if(config == "xymin"){
    plot_temp = plot_temp +
      geom_rug(data = rugdat, aes(x = predictor, y = logprog), sides = "b") +
      ylab("") +
      xlab(lab2) +
      theme_bw() +
      theme(axis.text.y=element_text(size = 12),
            axis.text.x=element_text(size = 12)) +
      theme(plot.margin = unit(c(0.1,0.1,0,-0.4), "cm"))
    print("config x")
  } else if(grepl("basic", config) == TRUE){
    plot_temp = plot_temp +
      xlab("") +
      ylab("") +
      theme_bw() +
      theme(axis.ticks.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.y=element_blank(),
            axis.title.y = 
              element_text(margin = 
                             ggplot2::margin( 
                               l = -15)),
            axis.title.x = 
              element_text(margin = 
                             ggplot2::margin( 
                               b = -15))) + 
      theme(plot.margin = unit(c(0,0,0,0), "cm"))
    print("config basic")
  }
  
  if(grepl("legend", config) != T){
    plot_temp = plot_temp +
      theme(legend.position = "none")
    print("config no legend")
  } 
  return(plot_temp)
}

# call model ####
setwd("E:/chapter3/for GAMs")
# g = read.csv(paste0("ch3_forGAMs_poly_prefire", x, "_smote.csv"))
# g = read.csv(paste0("trainingdata_prefire", x, "_iso.csv"))
g = read.csv(paste0("trainingdata_prefire", x, "_iso_redo.csv"))
g$ffdi_cat = as.factor(g$ffdi_cat)
g$fire_reg = as.factor(g$fire_reg)

g$logprog = log(g$prog)

setwd("D:/chapter3/outputs/GAMs")
gam1 = readRDS(paste0(modelnom, ".rds"))
rugdat = gam1$model

ymin = 0
ymax = 0.8
rugdat.temp = "temp"
# cover_z_1 ####
rugdat.temp = rugdat |>
  dplyr::select(logprog,
                cover_z_1)
names(rugdat.temp)[2] = "predictor"
rugdat.temp$predictor = rugdat.temp$predictor*100 

# p1 = plot_smooth(gam1, view = "cover_z_1", n.grid = 1000, transform = exp)
# p1 = p1$fv
# 
# fit = data.frame(fit = p1$fit, ul = p1$ul, ll = p1$ll, x = p1$cover_z_1)
# fit$x = fit$x*100
# write.csv(fit, paste0(modelnom, "_cvrz_predictions.csv"), row.names = F)
fit = read.csv(paste0(modelnom, "_cvrz_predictions.csv"))

g1 = summedallfun(dat = fit, rugdat = rugdat.temp, config = "x", lab2 = "Median understorey cover (%)", 
                  # ymin = min(fit$ll), ymax = max(fit$ul),
                  ymin = ymin, ymax = ymax,
                  ab2 = max(fit$fit))

# elevation_sd ####
rugdat.temp = rugdat |>
  dplyr::select(logprog,
                elevation_sd)
names(rugdat.temp)[2] = "predictor"

# p1 = plot_smooth(gam1, view = "elevation_sd", n.grid = 1000, transform = exp)
# p1 = p1$fv
# 
# fit = data.frame(fit = p1$fit, ul = p1$ul, ll = p1$ll, x = p1$elevation_sd)
# write.csv(fit, paste0(modelnom, "_elev_predictions.csv"), row.names = F)
fit = read.csv(paste0(modelnom, "_elev_predictions.csv"))

g2 = summedallfun(dat = fit, rugdat = rugdat.temp, config = "xy", lab2 = "Standard deviation in elevation (m)", 
                  # ymin = min(fit$ll), ymax = max(fit$ul),
                  ymin = ymin, ymax = 13,
                  ab2 = max(fit$fit))

# stringybark ####
rugdat.temp = rugdat |>
  dplyr::select(logprog,
                stringybark)
names(rugdat.temp)[2] = "predictor"
rugdat.temp$predictor = rugdat.temp$predictor * 100

# p1 = plot_smooth(gam1, view = "stringybark", n.grid = 1000, transform = exp)
# p1 = p1$fv
# 
# fit = data.frame(fit = p1$fit, ul = p1$ul, ll = p1$ll, x = p1$stringybark)
# fit$x = fit$x * 100
# write.csv(fit, paste0(modelnom, "_stringy_predictions.csv"), row.names = F)
fit = read.csv(paste0(modelnom, "_stringy_predictions.csv"))

g3 = summedallfun(dat = fit, rugdat = rugdat.temp, config = "x", lab2 = "Median stringybark probability (%)", 
                  # ymin = min(fit$ll), ymax = max(fit$ul),
                  ymin = ymin, ymax = ymax,
                  ab2 = max(fit$fit))

# LFMC.1 ####
rugdat.temp = rugdat |>
  dplyr::select(logprog,
                LFMC.1)
names(rugdat.temp)[2] = "predictor"

# p1 = plot_smooth(gam1, view = "LFMC.1", n.grid = 1000, transform = exp)
# p1 = p1$fv
# 
# fit = data.frame(fit = p1$fit, ul = p1$ul, ll = p1$ll, x = p1$LFMC.1)
# write.csv(fit, paste0(modelnom, "_lfmc_predictions.csv"), row.names = F)
fit = read.csv(paste0(modelnom, "_lfmc_predictions.csv"))

g4 = summedallfun(dat = fit, rugdat = rugdat.temp, config = "xy", lab2 = expression("Live fuel moisture content (10"^th*" percentile)"), 
                  # ymin = min(fit$ll), ymax = max(fit$ul),
                  ymin = ymin, ymax = ymax,
                  ab2 = max(fit$fit))

# VPD ####
rugdat.temp = rugdat |>
  dplyr::select(logprog,
                VPD)
names(rugdat.temp)[2] = "predictor"

# p1 = plot_smooth(gam1, view = "VPD", n.grid = 1000, transform = exp)
# p1 = p1$fv
# 
# fit = data.frame(fit = p1$fit, ul = p1$ul, ll = p1$ll, x = p1$VPD)
# write.csv(fit, paste0(modelnom, "_vpd_predictions.csv"), row.names = F)
fit = read.csv(paste0(modelnom, "_vpd_predictions.csv"))

g5 = summedallfun(dat = fit, rugdat = rugdat.temp, config = "x", lab2 = "Median vapour pressure deficit (kPa)", 
                  # ymin = min(fit$ll), ymax = max(fit$ul),
                  ymin = ymin, ymax = ymax,
                  ab2 = max(fit$fit))

# # windgust ####
# rugdat.temp = rugdat |>
#   dplyr::select(logprog,
#                 windgust)
# names(rugdat.temp)[2] = "predictor"
# 
# # p1 = plot_smooth(gam1, view = "windgust", n.grid = 1000, transform = exp)
# # p1 = p1$fv
# # 
# # fit = data.frame(fit = p1$fit, ul = p1$ul, ll = p1$ll, x = p1$windgust)
# # write.csv(fit, paste0(modelnom, "_windsgust_predictions.csv"), row.names = F)
# fit = read.csv(paste0(modelnom, "_windsgust_predictions.csv"))
# 
# g6 = summedallfun(dat = fit, rugdat = rugdat.temp, config = "x", lab2 = "Median wind gust speed (km/hr)", 
#                   # ymin = min(fit$ll), ymax = max(fit$ul),
#                   ymin = ymin, ymax = ymax,
#                   ab2 = max(fit$fit))
# 
# windspeed.stdev ####
rugdat.temp = rugdat |>
  dplyr::select(logprog,
                windspeed.stdev)
names(rugdat.temp)[2] = "predictor"

# p1 = plot_smooth(gam1, view = "windspeed.stdev", n.grid = 1000, transform = exp)
# p1 = p1$fv
# 
# fit = data.frame(fit = p1$fit, ul = p1$ul, ll = p1$ll, x = p1$windspeed.stdev)
# write.csv(fit, paste0(modelnom, "_windspeedstdev_predictions.csv"), row.names = F)
fit = read.csv(paste0(modelnom, "_windspeedstdev_predictions.csv"))

g7 = summedallfun(dat = fit, rugdat = rugdat.temp, config = "xy", lab2 = "Standard deviation in wind speed (km/hr)", 
                  # ymin = min(fit$ll), ymax = max(fit$ul),
                  ymin = ymin, ymax = ymax,
                  ab2 = max(fit$fit))

# winddir.stdev ####
rugdat.temp = rugdat |>
  dplyr::select(logprog,
                winddir.stdev)
names(rugdat.temp)[2] = "predictor"
rugdat.temp = rugdat.temp |> 
  filter(predictor < 147)

# p1 = plot_smooth(gam1, view = "winddir.stdev", n.grid = 1000, transform = exp)
# p1 = p1$fv
# 
# fit = data.frame(fit = p1$fit, ul = p1$ul, ll = p1$ll, x = p1$winddir.stdev)
# write.csv(fit, paste0(modelnom, "_winddirstdev_predictions.csv"), row.names = F)
fit = read.csv(paste0(modelnom, "_winddirstdev_predictions.csv"))

g8 = summedallfun(dat = fit, rugdat = rugdat.temp, config = "x", lab2 = "Standard deviation in wind direction (degrees)", 
                  # ymin = min(fit$ll), ymax = max(fit$ul),
                  ymin = ymin, ymax = ymax,
                  ab2 = max(fit$fit))


# water2 ####
rugdat.temp = rugdat |>
  dplyr::select(logprog,
                water2)
names(rugdat.temp)[2] = "predictor"
rugdat.temp$predictor = rugdat.temp$predictor/1000

# p1 = plot_smooth(gam1, view = "water2", n.grid = 1000, transform = exp)
# p1 = p1$fv
# 
# fit = data.frame(fit = p1$fit, ul = p1$ul, ll = p1$ll, x = p1$water2)
# write.csv(fit, paste0(modelnom, "_water_predictions.csv"), row.names = F)
fit = read.csv(paste0(modelnom, "_water_predictions.csv"))
fit$x = fit$x/1000

g9 = summedallfun(dat = fit, rugdat = rugdat.temp, config = "xymin", lab2 = "Median distance to water (km)", 
                  # ymin = min(fit$ll), ymax = max(fit$ul),
                  ymin = ymin, ymax = ymax,
                  ab2 = max(fit$fit))
# plots ####

setwd("D:/chapter3/outputs/GAMs")
plots = 
  (plot_grid(g2, g9, g6, g7, g8, g5, g4, g1, g3, align = "hv", axis = "tblr")) +
  plot_annotation(tag_levels = 'a') & 
  theme(plot.tag = element_text(size = 12))
ggexport(plots, filename = paste0("GAMiso_redo.jpeg"), width = 3000, height = 3000, res = 250)

ggexport(g1, filename = paste0("understorey_redo.png"), width = 1000, height = 1000, res = 250)
ggexport(g2, filename = paste0("elevation_redo.png"), width = 1000, height = 1000, res = 250)
ggexport(g3, filename = paste0("stringybark_redo.png"), width = 1000, height = 1000, res = 250)
ggexport(g4, filename = paste0("LFMC_redo.png"), width = 1000, height = 1000, res = 250)
ggexport(g5, filename = paste0("VPD_redo.png"), width = 1000, height = 1000, res = 250)
ggexport(g6, filename = paste0("windgust_redo.png"), width = 1000, height = 1000, res = 250)
ggexport(g7, filename = paste0("windspeed.stdev_redo.png"), width = 1000, height = 1000, res = 250)
ggexport(g8, filename = paste0("winddir.stdev_redo.png"), width = 1000, height = 1000, res = 250)
ggexport(g9, filename = paste0("water_redo.png"), width = 1000, height = 1000, res = 250)

## set zoom to 100%
g2
g9

g7
g8
g5

g4
g1
g3

# maps ####
setwd("D:/chapter1/bark-type-SDM/data")
nsw = st_read("NSW_sans_islands.shp")
nsw_buff = st_buffer(nsw, dist = 50000)
aus = readRDS("gadm36_AUS_1_sp.rds")
setwd("E:/chapter3/for GAMs")
# g = st_read(paste0("ch3_forGAMs_poly_prefire", x, "_final.gpkg"))
g = st_read(paste0("ch3_forGAMs_poly_prefire", x, "_final_redo.gpkg"))
setwd("D:/chapter1/other_data/Final/terrain variables")
# hill2 = raster("proj_dem_hillshade_30m.tif")
# hill2 = aggregate(hill2, fact = 10)
# hillproj = projectRaster(hill2, crs = "EPSG:4326")
# hillproj = mask(hillproj, nsw)
# writeRaster(hillproj, "hillshadeformapping.tif", overWrite = T)
hill2 = raster("hillshadeformapping.tif")

bb = st_buffer(g, dist = 50000)

tmap_options(check.and.fix = T)
tmap_mode("plot")
t1 =
  tm_shape(aus, bbox = bb) +
  tm_fill(col = gray(75/100)) +
  tm_shape(hill2) +
  tm_raster(palette = gray(50:100 / 100), n = 100, legend.show = FALSE) +
  # tm_shape(nsw) +
  # tm_borders() +
  tm_shape(g) + tm_fill(col = "prog", palette = turbo(20)[14:20], legend.show = FALSE) +
  tm_graticules(lines = F) + 
  tm_layout(inner.margins = 0)
# t1

setwd("D:/chapter3/outputs/GAMs")
# tmap_save(t1, "firepolygons.jpg", height = 3000, width = 2000, units = "px")
tmap_save(t1, "firepolygons_redo.jpg", height = 3000, width = 2000, units = "px")

t1 =
  tm_shape(g) + tm_fill(col = "prog", palette = turbo(20)[14:20])
t1

setwd("D:/chapter3/outputs/GAMs")
tmap_save(t1, "polygons_legend.jpg", height = 3000, width = 2000, units = "px")

# t1 =
  tm_shape(aus, bbox = bb) +
  tm_fill(col = gray(75/100)) +
  tm_shape(hill2) +
  tm_raster(palette = gray(50:100 / 100), n = 100, legend.show = FALSE) +
  # tm_shape(nsw) +
  # tm_borders() +
  tm_shape(g) + tm_fill(col = "wind.stdev", palette = turbo(20)[14:20], legend.show = FALSE) +
  tm_graticules(lines = F) + 
  tm_layout(inner.margins = 0)
# t1