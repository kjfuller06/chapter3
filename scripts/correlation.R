library(sf)
library(tidyverse)
library(psych)
library(corrplot)
library(raster)
library(exactextractr)
library(tmap)
library(viridis)

# cor plots ####
setwd("E:/chapter3/for GAMs")
dat = st_read("ch3_forGAMs_poly_prefire180_final_redo.gpkg")
st_geometry(dat) = NULL

dat$breaks = apply(dat |> dplyr::select(firelines, roads), 1, FUN = min, na.rm = T)

dat2 = dat |> dplyr::select(elevation_sd, water2, water3, water4, breaks, windspeed, windspeed.1, windspeed.9, windspeed.stdev, windgust, windgust.9, windgust.stdev, winddir.stdev, VPD, VPD.1, VPD.9, LFMC, LFMC.1, LFMC.9, rh98:fhd_normal, stringybark, ribbonbark)
names(dat2) = c("Standard devation in elevation",
             "Median distance to top two orders of\nwater features",
             "Median distance to top three orders of\nwater features",
             "Median distance to top four orders of\nwater features",
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
c1 = cor(dat2, method = "spearman") 
c1 |> 
  corrplot(
    order="original", 
    method = "circle", na.label = "NA",
    addCoef.col ='black',
    tl.col = "black",
    tl.srt = 45,
    number.cex = 0.5)
dev.off()

dat2 = dat |> dplyr::select(elevation_sd, water2, water3, water4, breaks, windspeed, windspeed.stdev, windgust, windgust.stdev, winddir.stdev, VPD, LFMC, rh98, cover_z_1, over_cover, fhd_normal, stringybark, ribbonbark)
names(dat2) = c("Standard devation in elevation",
                "Median distance to two orders of water features",
                "Median distance to three orders of water features",
                "Median distance to four orders of water features",
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
jpeg("corr_target.jpeg", width = 4000, height = 3250, res = 300)
c1 = cor(dat2, method = "spearman") 
c1 |> 
  corrplot(
    order="original", 
    method = "circle", na.label = "NA",
    addCoef.col ='black',
    tl.col = "black",
    tl.srt = 45,
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

# wind direction and wind speed ####
setwd("E:/chapter3/for GAMs")
dat = st_read("ch3_forGAMs_poly_prefire180_final_redo.gpkg")
targetcrs = st_crs(dat)

dat2 = dat |>
  filter(winddir.stdev < 25)
g1 = dat2[sample(c(1:nrow(dat2)), size = 10),]
dat2 = dat |>
  filter(winddir.stdev > 75 & winddir.stdev < 110)
g2 = dat2[sample(c(1:nrow(dat2)), size = 10),]
g = rbind(g1, g2) |>
  dplyr::select(ID,
                poly_sD,
                poly_eD)

g_buffer = st_buffer(g, dist = 100000)

# setwd("/glade/scratch/kjfuller/data")
setwd("E:/chapter3/from Michael")
wind = st_read("wind_direction.gpkg")
wind = st_transform(wind, crs = st_crs(g))

stations = wind |>
  dplyr::select(station)
stations = stations[!duplicated(stations$station),]
st_geometry(wind) = NULL

counter <- 0
l <- list(NULL)
size <- 1
windfun = function(x){
  if( .GlobalEnv$counter == .GlobalEnv$size )
  {length(.GlobalEnv$l) <- .GlobalEnv$size <- .GlobalEnv$size * 2}
  
  tryCatch({
    # select a buffered polygon and index all stations within 100km
    g_temp = g_buffer[x,]
    stat_temp = stations[g_temp,]
    
    # g_center = g |>
    #   filter(ID %in% g_temp$ID) |>
    #   st_centroid()
    # tmap_mode("view")
    # tm_shape(g_temp) + tm_borders() + tm_shape(stat_temp) + tm_dots() + tm_shape(g_center) + tm_dots(col = "blue")
    
    # select the original polygon and calculate distance from centroid to selected stations
    g_temp = g |>
      filter(ID %in% g_temp$ID)
    g_center = st_centroid(g_temp)
    dist = as.data.frame(st_distance(g_temp, stat_temp))
    st_geometry(stat_temp) = NULL
    st_geometry(g_temp) = NULL
    stat_temp = cbind(stat_temp, t(dist))
    
    # select wind data based on polygon timeframe
    wind_temp = wind |>
      filter(station %in% stat_temp$station) |>
      left_join(stat_temp)
    minT = wind_temp |>
      filter(DateTime <= min(g_temp$poly_sD))
    minT = max(minT$DateTime, na.rm = T)
    maxT = wind_temp |>
      filter(DateTime >= max(g_temp$poly_eD))
    maxT = min(maxT$DateTime, na.rm = T)
    wind_temp = wind_temp |>
      filter(DateTime >= minT) |>
      filter(DateTime <= maxT)
    names(wind_temp)[ncol(wind_temp)] = "weights"
    wind_temp$weights[wind_temp$weights > 100000] = 100000
    wind_temp$weights = 1 - (wind_temp$weights/100000)
    
    # calculate the median wind speed within the polygon timeframe at each selected station, join to station data
    windspeed = as.numeric(lapply(unique(wind_temp$DateTime),
                                        FUN = function(x) with(wind_temp |> filter(DateTime == x), weighted.mean(windspeed, weights, na.rm = T))))
    windgust = as.numeric(lapply(unique(wind_temp$DateTime),
                                  FUN = function(x) with(wind_temp |> filter(DateTime == x), weighted.mean(windgust, weights, na.rm = T))))
    winddir = as.numeric(lapply(unique(wind_temp$DateTime),
                                 FUN = function(x) with(wind_temp |> filter(DateTime == x), weighted.mean(winddir, weights, na.rm = T))))
    
    g_temp = data.frame(ID = unique(g_temp$ID), 
                          times = c(1:length(unique(wind_temp$DateTime))), 
                          windspeed = windspeed, 
                          windspeed.stdev = sd(windspeed, na.rm = T), 
                          windgust = windgust, 
                          windgust.stdev = sd(windgust, na.rm = T),
                          winddir = winddir,
                          winddir.stdev = sd(winddir, na.rm = T))
    
    .GlobalEnv$counter <- .GlobalEnv$counter + 1
    .GlobalEnv$l[[.GlobalEnv$counter]] <- g_temp
    
  }, error = function(e){print(x); cat("ERROR :", conditionMessage(e), "\n")})
}
for(i in c(1:nrow(g_buffer))){
  windfun(i)
  print(i)
}
print("binding rows")
g_temp = bind_rows(l)
setwd("D:/chapter3/outputs/GAMs")
write.csv(g_temp, "wind_sequence.csv", row.names = F)

print("joining to original isochron data")
g = left_join(g, g_temp)
# st_write(g, "test.gpkg", delete_dsn = T)
st_write(g, paste0("wind_sequence.gpkg"), delete_dsn = T)
g = st_read("wind_sequence.gpkg")
st_geometry(g) = NULL

ggplot(g, aes(x = times, y = windspeed, group = ID)) +
  geom_line(aes(col = windspeed.stdev))
ggplot(g, aes(x = times, y = windspeed, group = ID)) +
  geom_line(aes(col = winddir.stdev))

g2 = aggregate(data = g2, windspeed ~ ID, FUN = max)
names(g2)[2] = "windspeed.max"
g2 = left_join(g2, g2)
ggplot(g2, aes(x = windspeed.stdev, y = winddir.stdev)) +
  geom_point(aes(col = windspeed.max))

ggplot(dat, aes(x = windspeed.stdev, y = winddir.stdev)) +
  geom_point(aes(col = windspeed.9))

dat2 = dat |> 
  dplyr::select(ID, windspeed.stdev, winddir.stdev, windspeed.9) |> 
  unique()
gam1 = mgcv::gam(data = dat2, windspeed.9 ~ windspeed.stdev*winddir.stdev, method = "REML")
summary(gam1)
par(mfrow = c(2, 2))
gam.check(gam1)

# understorey cover and stringybarks ####
setwd("E:/chapter3/for GAMs")
x = 180
modelnom = paste0("ch3_GAM_iso_prefire", x, "_redo")
# g = read.csv(paste0("ch3_forGAMs_poly_prefire", x, "_smote.csv"))
# g = read.csv(paste0("trainingdata_prefire", x, "_iso.csv"))
g = read.csv(paste0("trainingdata_prefire", x, "_iso_redo.csv"))
g$ffdi_cat = as.factor(g$ffdi_cat)
g$fire_reg = as.factor(g$fire_reg)
g$logprog = log(g$prog)
levels(g$fire_reg) = c("Rainforests",
                       "Wet sclerophyll forests\n(shrubby understorey)",
                       "Wet sclerophyll forests\n(grassy understorey)",
                       "Dry sclerophyll forests\n(shrub/grass understorey)",
                       "Dry sclerophyll forests\n(shrubby understorey)",
                       "Grassy woodlands",
                       "Heathlands",
                       "Forested wetlands")
g$cover_z_1 = g$cover_z_1*100
g$stringybark = g$stringybark*100

setwd("D:/chapter3/outputs/GAMs")
gam1 = readRDS(paste0(modelnom, ".rds"))

test.temp = expand.grid(cover_z_1 = seq(0, 1, length.out = 50),
                        elevation_sd = median(g$elevation_sd),
                        stringybark = median(g$stringybark),
                        LFMC.1 = seq(50, 130, length.out = 50),
                        VPD = median(g$VPD),
                        winddir.stdev = median(g$winddir.stdev),
                        windspeed.stdev = median(g$windspeed.stdev),
                        water2 = median(g$water2))

preds<-predict(gam1, type="terms", newdata=test.temp,
               se.fit=TRUE)

x = test.temp[,1]
preds.temp = rowSums(preds$fit) + coef(gam1)[1]
fit = data.frame(fit = exp(preds.temp), x = x)
fit$y = test.temp$LFMC.1
fit$x = fit$x*100

g1 = ggplot(fit, aes(x=x, y=y)) + 
  geom_tile(aes(fill = fit)) +
  # scale_fill_viridis(palette = turbo) +
  scale_fill_gradientn(colors = turbo(20)[14:20], name = "Fire rate of\nprogression\n(km\u00B2/hr)") +
  geom_density_2d(data = g, aes(x = cover_z_1, y = LFMC.1), contour_var = "ndensity", col = "white", breaks = c(0, 0.5, 0.7, 0.9)) +
  facet_wrap(facets = "fire_reg") +
  theme_bw() +
  xlab("Median understorey cover (%)") +
  ylab("Live fuel moisture content (10th percentile)")
g1
ggsave(plot = g1, "coverz1-LFMC.jpg", device = "jpg", height = 2000, width = 2000, units = "px")

test.temp = expand.grid(cover_z_1 = seq(0, 1, length.out = 50),
                        elevation_sd = median(g$elevation_sd),
                        stringybark = median(g$stringybark),
                        LFMC.1 = median(g$LFMC.1),
                        VPD = seq(min(g$VPD), 7.5, length.out = 50),
                        winddir.stdev = median(g$winddir.stdev),
                        windspeed.stdev = median(g$windspeed.stdev),
                        water2 = median(g$water2))

preds<-predict(gam1, type="terms", newdata=test.temp,
               se.fit=TRUE)

x = test.temp[,1]
preds.temp = rowSums(preds$fit) + coef(gam1)[1]
fit = data.frame(fit = exp(preds.temp), x = x)
fit$y = test.temp$VPD
fit$x = fit$x*100

g1 = ggplot(fit, aes(x=x, y=y)) + 
  geom_tile(aes(fill = fit)) +
  # scale_fill_viridis(palette = turbo) +
  scale_fill_gradientn(colors = turbo(20)[14:20], name = "Fire rate of\nprogression\n(km\u00B2/hr)") +
  geom_density_2d(data = g, aes(x = cover_z_1, y = VPD), contour_var = "ndensity", col = "white", breaks = c(0, 0.6, 0.7, 0.8, 0.9, 1)) +
  facet_wrap(facets = "fire_reg") +
  theme_bw() +
  xlab("Understorey Cover (%)") +
  ylab("Vapour pressure deficit (kPa)")
g1
ggsave(plot = g1, "coverz1-VPD.jpg", device = "jpg", height = 2000, width = 2000, units = "px")

ggplot(g, aes(x = VPD.9, cover_z_1)) +
  geom_point() 
  # facet_wrap(facets = "fire_reg")

cor(g |> dplyr::select(VPD, VPD.1, VPD.9, LFMC, LFMC.1, LFMC.9, cover_z_1))

par(mfrow = c(1, 2))
with(g |> filter(fire_reg == "Heathlands"), hist(cover_z_1))
with(g |> filter(fire_reg != "Heathlands"), hist(cover_z_1))

par(mfrow = c(1, 2))
with(g |> filter(fire_reg == "Rainforests"), hist(cover_z_1))
with(g |> filter(fire_reg != "Rainforests"), hist(cover_z_1))

test.temp = expand.grid(cover_z_1 = median(g$cover_z_1),
                        elevation_sd = median(g$elevation_sd),
                        stringybark = seq(0, 1, length.out = 50),
                        LFMC.1 = seq(min(g$LFMC.1), max(g$LFMC.1), length.out = 50),
                        VPD = median(g$VPD),
                        winddir.stdev = median(g$winddir.stdev),
                        windspeed.stdev = median(g$windspeed.stdev),
                        water2 = median(g$water2))

preds<-predict(gam1, type="terms", newdata=test.temp,
               se.fit=TRUE)

x = test.temp[,4]
preds.temp = rowSums(preds$fit) + coef(gam1)[1]
fit = data.frame(fit = exp(preds.temp), x = x)
fit$y = test.temp$stringybark
fit$y = fit$y*100

g1 = ggplot(fit, aes(x=x, y=y)) + 
  geom_tile(aes(fill = fit)) +
  # scale_fill_viridis(palette = turbo) +
  scale_fill_gradientn(colors = turbo(20)[14:20], name = "Fire rate of\nprogression\n(km\u00B2/hr)") +
  geom_density_2d(data = g, aes(x = LFMC.1, y = stringybark), contour_var = "ndensity", col = "white", breaks = c(0, 0.6, 0.7, 0.8, 0.9, 1)) +
  facet_wrap(facets = "fire_reg") +
  theme_bw() +
  xlab("Median understorey cover (%)") +
  ylab("Median stringybark probability (%)")
g1

par(mfrow = c(1, 2))
with(g |> filter(fire_reg == "Rainforests"), hist(stringybark))
with(g |> filter(fire_reg != "Rainforests"), hist(stringybark))

par(mfrow = c(1, 2))
with(g |> filter(fire_reg == "Heathlands"), hist(stringybark))
with(g |> filter(fire_reg != "Heathlands"), hist(stringybark))

g1 = ggplot(g, aes(stringybark, col = fire_reg)) +
  geom_density() +
  theme(legend.position = "none")
g2 = ggplot(g, aes(logprog, col = fire_reg)) +
  geom_density() +
  theme(legend.position = "none")
g1 | g2

ggplot(g, aes(x = stringybark, y = prog)) +
  geom_smooth() +
  facet_wrap(facets = "fire_reg", scales = "free_y")
