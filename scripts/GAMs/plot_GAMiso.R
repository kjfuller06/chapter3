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
            axis.text.x=element_text(size = 12))
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
                               b = -15)))
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
                               l = -15)))
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
                               b = -15)))
    print("config basic")
  }
  
  if(grepl("legend", config) != T){
    plot_temp = plot_temp +
      theme(legend.position = "none")
    print("config no legend")
  } 
  return(plot_temp)
}
# plotfun = function(b1, config, byspei, ymin, ymax, dat, rugdat, lab2, ab2 = ab2, binwidth, breaks){
#   if(grepl("cat", config) == TRUE){
#     plot_temp = ggplot(dat) +
#       geom_boxplot(aes(x = x, middle = fit, ymin = ll, lower = ll, upper = ul, ymax = ul, color = x, fill = x), stat= "identity", alpha = 0.15) +
#       scale_colour_manual(values = spei.list, name = lab2) +
#       scale_fill_manual(values = spei.list, name = lab2) +
#       geom_point(aes(x = x, y = fit))
#   } else if(grepl("continuous", config) == T) {
#     plot_temp = ggplot(dat, aes(x=TSF, y=PC1)) + 
#       geom_tile(aes(fill = fit)) +
#       scale_fill_gradient(low = "grey70", high = "darkgreen", name = lab) +
#       geom_contour(aes(z = fit), colour = "white", binwidth = binwidth) +
#       geom_text_contour(aes(z = fit), skip = 0, breaks = breaks, colour = "black", check_overlap = T, rotate = F) +
#       theme_bw()
#     if(grepl("legend", config) == T){
#       plot_temp = plot_temp + 
#         xlab("TSF (years)") +
#         geom_rug(data = rugdat, aes(x = predictor, y = response), sides = "bl") + 
#         theme(legend.position="right") 
#     } else if(grepl("legend", config) == F){
#       plot_temp = plot_temp +
#         theme(legend.position="right", 
#                                     axis.ticks.x=element_blank(),
#                                     axis.text.x=element_blank(),
#                                     axis.title.x = 
#                                       element_text(margin = 
#                                                      ggplot2::margin( 
#                                                        b = -15))) 
#     }
#     
#   } else if(byspei == F){
#     plot_temp = ggplot(dat, aes(x = x, y = fit)) +
#       geom_line(color = "black") +
#       geom_ribbon(aes(ymin=ll, ymax=ul), alpha=0.15, fill = "black") +
#       geom_rug(data = rugdat, aes(x = predictor, y = response), sides = "b")
#     
#   } else if(byspei == T){
#     plot_temp = ggplot(dat, aes(x = x, y = fit)) +
#       geom_line(aes(color = speifact)) +
#       scale_color_manual(values = spei.list, name = "SPEI") +
#       geom_ribbon(aes(ymin=ll, ymax=ul, fill = speifact), alpha=0.05) +
#       scale_fill_manual(values = spei.list, name = "SPEI") +
#       geom_rug(data = rugdat, aes(x = predictor, y = response), sides = "b")
#   }
#   
#   if(config == "firstline"){
#     plot_temp = plot_temp +
#       ylab(lab) +
#       xlab(lab2) +
#       coord_cartesian(ylim = c(ymin, ymax)) +
#       geom_abline(slope = 0, intercept = 0, lty = 2) +
#       geom_abline(slope = 0, intercept = ab2, lty = 2) +
#       theme_bw() +
#       theme(legend.position="none",
#             axis.text.y=element_text(size = 12),
#             axis.text.x=element_text(size = 12))
#     
#   } else if(config == "legendline"){
#     plot_temp = plot_temp +
#       ylab(lab) +
#       xlab(lab2) +
#       coord_cartesian(ylim = c(ymin, ymax)) +
#       geom_abline(slope = 0, intercept = 0, lty = 2) +
#       geom_abline(slope = 0, intercept = ab2, lty = 2) +
#       theme_bw() +
#       theme(axis.text.y=element_text(size = 12),
#             axis.text.x=element_text(size = 12),
#             axis.title.y = 
#               element_text(margin = 
#                              ggplot2::margin( 
#                                l = -15)))
#     
#   } else if(config == "basicline"){
#     plot_temp = plot_temp +
#       xlab(lab2) +
#       ylab("") +
#       coord_cartesian(ylim = c(ymin, ymax)) +
#       geom_abline(slope = 0, intercept = 0, lty = 2) +
#       geom_abline(slope = 0, intercept = ab2, lty = 2) +
#       theme_bw() +
#       theme(legend.position="none",
#             axis.text.y=element_text(size = 12),
#             axis.text.x=element_text(size = 12),
#             axis.title.y = 
#               element_text(margin = 
#                              ggplot2::margin( 
#                                l = -15)))
#     
#   } else if(config == "basiccat"){
#     plot_temp = plot_temp +
#       ylab("") +
#       xlab(lab2) +
#       coord_cartesian(ylim = c(ymin, ymax)) +
#       theme_bw() +
#       theme(legend.position="none",
#             axis.text.y=element_text(size = 12),
#             axis.ticks.x=element_blank(),
#             axis.text.x=element_blank(),
#             axis.title.y = 
#               element_text(margin = 
#                              ggplot2::margin( 
#                                l = -15)))
#     
#   } 
#   return(plot_temp)
# }

# call model ####
setwd("E:/chapter3/for GAMs")
# g = read.csv(paste0("ch3_forGAMs_poly_prefire", x, "_smote.csv"))
g = read.csv(paste0("trainingdata_prefire", x, "_iso.csv"))
g$ffdi_cat = as.factor(g$ffdi_cat)
g$fire_reg = as.factor(g$fire_reg)

g$logprog = log(g$prog)

setwd("D:/chapter3/outputs/GAMs")
gam1 = readRDS(paste0(modelnom, ".rds"))
rugdat = gam1$model

ymin = 0
ymax = 1.1
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

g1 = summedallfun(dat = fit, rugdat = rugdat.temp, config = "xy", lab2 = "Median understorey cover (%)", 
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
                  ymin = ymin, ymax = 8.75,
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

g3 = summedallfun(dat = fit, rugdat = rugdat.temp, config = "xy", lab2 = "Median stringybark probability (%)", 
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

g5 = summedallfun(dat = fit, rugdat = rugdat.temp, config = "xy", lab2 = "Median vapour pressure deficit (kPa)", 
                  # ymin = min(fit$ll), ymax = max(fit$ul),
                  ymin = ymin, ymax = ymax,
                  ab2 = max(fit$fit))

# wind.stdev ####
rugdat.temp = rugdat |>
  dplyr::select(logprog,
                wind.stdev)
names(rugdat.temp)[2] = "predictor"

# p1 = plot_smooth(gam1, view = "wind.stdev", n.grid = 1000, transform = exp)
# p1 = p1$fv
# 
# fit = data.frame(fit = p1$fit, ul = p1$ul, ll = p1$ll, x = p1$wind.stdev)
# write.csv(fit, paste0(modelnom, "_windstdev_predictions.csv"), row.names = F)
fit = read.csv(paste0(modelnom, "_windstdev_predictions.csv"))

g6 = summedallfun(dat = fit, rugdat = rugdat.temp, config = "xy", lab2 = "Standard deviation in wind direction (radians)", 
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

g7 = summedallfun(dat = fit, rugdat = rugdat.temp, config = "xy", lab2 = "Median distance to water (km)", 
                  # ymin = min(fit$ll), ymax = max(fit$ul),
                  ymin = ymin, ymax = ymax,
                  ab2 = max(fit$fit))
# plots ####
# (g2 + g1 + g3)/(g4 | g5 | g6 | g7)

setwd("D:/chapter3/outputs/GAMs")
plots = 
  (g2 | g1 | g3 | g4 | g5 | g6 | g7) +
  plot_annotation(tag_levels = 'a') & 
  theme(plot.tag = element_text(size = 12))
ggexport(plots, filename = paste0("GAMiso.jpeg"), width = 7000, height = 1500, res = 250)

ggexport(g1, filename = paste0("understorey.png"), width = 1000, height = 1000, res = 250)
ggexport(g2, filename = paste0("elevation.png"), width = 1000, height = 1000, res = 250)
ggexport(g3, filename = paste0("stringybark.png"), width = 1000, height = 1000, res = 250)
ggexport(g4, filename = paste0("LFMC.png"), width = 1000, height = 1000, res = 250)
ggexport(g5, filename = paste0("VPD.png"), width = 1000, height = 1000, res = 250)
ggexport(g6, filename = paste0("wind.stdev.png"), width = 1000, height = 1000, res = 250)
ggexport(g7, filename = paste0("water.png"), width = 1000, height = 1000, res = 250)

## set zoom to 175%
g2
g7

g6
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
g = st_read(paste0("ch3_forGAMs_poly_prefire", x, "_final.gpkg"))
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
tmap_save(t1, "firepolygons.jpg", height = 3000, width = 2000, units = "px")

t1 =
  tm_shape(g) + tm_fill(col = "prog", palette = turbo(20)[14:20])
t1

setwd("D:/chapter3/outputs/GAMs")
tmap_save(t1, "polygons_legend.jpg", height = 3000, width = 2000, units = "px")
