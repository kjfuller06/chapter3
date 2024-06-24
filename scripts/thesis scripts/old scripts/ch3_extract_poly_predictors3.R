library(sf)
library(raster)
library(tidyverse)
library(exactextractr)
library(rgeos)

# wind quantiles ####
# setwd("/glade/scratch/kjfuller/data/chapter3")
setwd("E:/chapter3/for GAMs")
g = st_read("ch3_forGAMs_poly_prefire180_final_redo.gpkg")
targetcrs = st_crs(g)
g = g |>
  dplyr::select(ID,
                poly_sD,
                poly_eD)

index = g |>
  dplyr::select(poly_sD,
                poly_eD)
st_geometry(index) = NULL
index = unique(index)
index$index = c(1:nrow(index))
g = g |>
  left_join(index)

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
    g_temp = g_buffer[g_buffer$index == unique(g_buffer$index)[x],]
    stat_temp = stations[g_temp,]

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

    # calculate the median wind speed within the polygon timeframe at each selected station, join to station data
    windsp1 = aggregate(data = wind_temp, windspeed ~ station, FUN = function(x) sd(x, na.rm = T))
    names(windsp1)[2] = "windspeed.stdev"
    windsp2 = aggregate(data = wind_temp, windspeed ~ station, FUN = function(x) median(x, na.rm = T))
    names(windsp2)[2] = "windspeed"
    windsp3 = aggregate(data = wind_temp, windspeed ~ station, FUN = function(x) quantile(x, probs = 0.1, na.rm = T))
    names(windsp3)[2] = "windspeed.1"
    windsp4 = aggregate(data = wind_temp, windspeed ~ station, FUN = function(x) quantile(x, probs = 0.9, na.rm = T))
    names(windsp4)[2] = "windspeed.9"

    windgt1 = aggregate(data = wind_temp, windgust ~ station, FUN = function(x) sd(x, na.rm = T))
    names(windgt1)[2] = "windgust.stdev"
    windgt2 = aggregate(data = wind_temp, windgust ~ station, FUN = function(x) median(x, na.rm = T))
    names(windgt2)[2] = "windgust"
    windgt3 = aggregate(data = wind_temp, windgust ~ station, FUN = function(x) quantile(x, probs = 0.9, na.rm = T))
    names(windgt3)[2] = "windgust.9"

    winddir1 = aggregate(data = wind_temp, winddir ~ station, FUN = function(x) median(x[x != 0], na.rm = T))
    names(winddir1)[2] = "winddir"
    winddir2 = aggregate(data = wind_temp, winddir ~ station, FUN = function(x) sd(x[x != 0], na.rm = T))
    names(winddir2)[2] = "winddir.stdev"

    wind_temp = stat_temp |>
      full_join(windsp1) |>
      full_join(windsp2) |>
      full_join(windsp3) |>
      full_join(windsp4) |>
      full_join(windgt1) |>
      full_join(windgt2) |>
      full_join(windgt3) |>
      full_join(winddir1) |>
      full_join(winddir2)

    weights = wind_temp |> dplyr::select(c(2:(ncol(wind_temp) - 9)))
    weights[weights > 100000] = 100000

    # create distance-based weights and calculate distance-weighted mean and wind impact index
    weights = as.data.frame(lapply(weights, function(x) {1 - (x/100000)}))
    g_temp$windspeed.stdev = as.numeric(lapply(weights,
                                               function(x) {weighted.mean(wind_temp[,"windspeed.stdev"], x, na.rm = T)}))
    g_temp$windspeed = as.numeric(lapply(weights,
                                         function(x) {weighted.mean(wind_temp[,"windspeed"], x, na.rm = T)}))
    g_temp$windspeed.1 = as.numeric(lapply(weights,
                                         function(x) {weighted.mean(wind_temp[,"windspeed.1"], x, na.rm = T)}))
    g_temp$windspeed.9 = as.numeric(lapply(weights,
                                         function(x) {weighted.mean(wind_temp[,"windspeed.9"], x, na.rm = T)}))
    g_temp$windgust.stdev = as.numeric(lapply(weights,
                                        function(x) {weighted.mean(wind_temp[,"windgust.stdev"], x, na.rm = T)}))
    g_temp$windgust = as.numeric(lapply(weights,
                                              function(x) {weighted.mean(wind_temp[,"windgust"], x, na.rm = T)}))
    g_temp$windgust.9 = as.numeric(lapply(weights,
                                              function(x) {weighted.mean(wind_temp[,"windgust.9"], x, na.rm = T)}))
    g_temp$winddir.stdev = as.numeric(lapply(weights,
                                              function(x) {weighted.mean(wind_temp[,"winddir.stdev"], x, na.rm = T)}))
    g_temp$winddir = as.numeric(lapply(weights,
                                             function(x) {weighted.mean(wind_temp[,"winddir"], x, na.rm = T)}))

    .GlobalEnv$counter <- .GlobalEnv$counter + 1
    .GlobalEnv$l[[.GlobalEnv$counter]] <- g_temp

  }, error = function(e){print(x); cat("ERROR :", conditionMessage(e), "\n")})
}
for(i in c(1:length(unique(g_buffer$index)))){
  windfun(i)
  print(i)
}
print("binding rows")
g_temp = bind_rows(l)
setwd("E:/chapter3/for GAMs")
write.csv(g_temp, "ch3_forGAMs_poly_prefire180_wind4.csv", row.names = F)

print("joining to original isochron data")
g = left_join(g, g_temp)
st_write(g, paste0("ch3_forGAMs_poly_prefire180_wind4.gpkg"), delete_dsn = T)

# process all outputs ####
setwd("E:/chapter3/for GAMs")
g1 = st_read("ch3_forGAMs_poly_prefire180_structure_redo_2-14.gpkg")
g1$area = as.numeric((st_area(g1)/1000000))
g1$prog = as.numeric((st_area(g1)/1000000)/g1$progtime)

g2 = st_read("ch3_forGAMs_poly_prefire180_elevation.gpkg") |>
  dplyr::select(ID,
                elevation_sd,
                elevation_md)
g2_issue = g2[is.na(g2$elevation_sd),]
st_area(g1[g2$ID %in% g2_issue$ID,])
min(st_area(g1))
## issues result from very small polygons
g2$elevation_sd[is.na(g2$elevation_sd)] = 0
g2 = g2 |>
  dplyr::select(ID,
                elevation_sd)
st_geometry(g2) = NULL

g2.2 = st_read("ch3_forGAMs_poly_prefire180_elevation_redo_6-19.gpkg") |>
  dplyr::select(ID,
                elevation_sd)
g2_issue = g2.2[is.na(g2.2$elevation_sd),]
st_area(g1[g2.2$ID %in% g2_issue$ID,])
min(st_area(g1))
## issues result from very small polygons
g2.2$elevation_sd[is.na(g2.2$elevation_sd)] = 0
g2.2 = g2.2 |>
  dplyr::select(ID,
                elevation_sd)
st_geometry(g2.2) = NULL
names(g2.2)[2] = "elevation_se"
g2 = full_join(g2, g2.2) |> na.omit()
cor(g2, method = "spearman")

g3 = st_read("ch3_forGAMs_poly_prefire180_fueltype.gpkg") |>
  dplyr::select(ID,
                fueltype,
                fire_reg)

st_geometry(g3) = NULL
summary(g3)

g5 = st_read("ch3_forGAMs_poly_prefire180_aspect.gpkg")
st_geometry(g5) = NULL
summary(g5)
g6 = st_read("ch3_forGAMs_poly_prefire180_firelines.gpkg")
st_geometry(g6) = NULL
summary(g6)
g7 = st_read("ch3_forGAMs_poly_prefire180_roads.gpkg")
st_geometry(g7) = NULL
summary(g7)

g8.1 = st_read("ch3_forGAMs_poly_prefire180_hydro1_nonpere.gpkg")
st_geometry(g8.1) = NULL
summary(g8.1)
g8.2 = st_read("ch3_forGAMs_poly_prefire180_hydro2_nonpere.gpkg")
st_geometry(g8.2) = NULL
summary(g8.2)
g8 = st_read("ch3_forGAMs_poly_prefire180_hydro3_nonpere.gpkg")
st_geometry(g8) = NULL
summary(g8)
g9 = st_read("ch3_forGAMs_poly_prefire180_hydro4_nonpere.gpkg")
st_geometry(g9) = NULL
summary(g9)
g10 = st_read("ch3_forGAMs_poly_prefire180_hydro5_nonpere.gpkg")
st_geometry(g10) = NULL
summary(g10)
g10.1 = st_read("ch3_forGAMs_poly_prefire180_hydro6_nonpere.gpkg")
st_geometry(g10.1) = NULL
summary(g10.1)
g10.2 = st_read("ch3_forGAMs_poly_prefire180_hydro7_nonpere.gpkg")
st_geometry(g10.2) = NULL
summary(g10.2)
g10.3 = st_read("ch3_forGAMs_poly_prefire180_hydro8_nonpere.gpkg")
st_geometry(g10.3) = NULL
summary(g10.3)

g11 = st_read("ch3_forGAMs_poly_prefire180_bark1.2_redo.gpkg")
st_geometry(g11) = NULL
summary(g11)
g11 = g11 |>
  filter(!is.na(stringybark))
summary(g11)

g12 = st_read("ch3_forGAMs_poly_prefire180_bark2.2_redo.gpkg")
st_geometry(g12) = NULL
g12 = g12 |>
  filter(!is.na(ribbonbark))
summary(g12)

g13 = st_read("ch3_forGAMs_poly_prefire180_LFMC.gpkg")
st_geometry(g13) = NULL
summary(g13)
all(g13$poly_sD > g13$LFMC_sD)
## TRUE
g13 = g13 |>
  dplyr::select(ID,
                LFMC,
                LFMC.1,
                LFMC.9)
summary(g13)

g14 = st_read("ch3_forGAMs_poly_prefire180_dynamic.gpkg")
st_geometry(g14) = NULL
summary(g14)
all(g14$poly_sD >= g14$VPD_sD)
## TRUE
g14 = g14 |>
  dplyr::select(ID,
                VPD,
                VPD.1,
                VPD.9)
summary(g14)

g16 = st_read("ch3_forGAMs_poly_prefire180_wind4.gpkg")
st_geometry(g16) = NULL
summary(g16)
g16 = g16 |>
  filter(!is.na(windspeed.9)) |>
  filter(!is.na(winddir.stdev)) |>
  dplyr::select(ID,
                winddir,
                winddir.stdev,
                windspeed,
                windspeed.1,
                windspeed.9,
                windspeed.stdev,
                windgust,
                windgust.9,
                windgust.stdev)
summary(g16)
# issue = g16[g16$windspeed < 0,]
# ## checked issue in wind extraction code
any(g16$windspeed < g16$windspeed.1)
## FALSE
any(g16$windspeed > g16$windspeed.9)
## FALSE
any(g16$windspeed > g16$windgust)
## TRUE
g16$windgust[g16$windspeed > g16$windgust] = g16$windspeed[g16$windspeed > g16$windgust]
any(g16$windspeed > g16$windgust)
## FALSE
any(g16$windspeed > g16$windgust.9)
## FALSE
any(g16$winddir < 0 | g16$winddir > 359)
## FALSE
cor(g16 |> dplyr::select(windspeed.stdev, windgust.stdev, winddir.stdev), method = "spearman")

g = g1 |>
  left_join(g2) |>
  left_join(g3) |>
  # left_join(g4) |>
  left_join(g5) |>
  left_join(g6) |>
  left_join(g7) |>
  left_join(g8.1) |>
  left_join(g8.2) |>
  left_join(g8) |>
  left_join(g9) |>
  left_join(g10) |>
  left_join(g10.1) |>
  left_join(g10.2) |>
  left_join(g10.3) |>
  left_join(g11) |>
  left_join(g12) |>
  left_join(g13) |>
  left_join(g14) |>
  # left_join(g15) |>
  left_join(g16) |>
  filter(!is.na(fire_reg)) |>
  filter(!is.na(LFMC)) |>
  # filter(prog < 200) |>
  # filter(fire_reg < 16) |>
  filter(!is.na(windspeed.9)) |>
  filter(!is.na(winddir.stdev)) |>
  filter(!is.na(windgust.stdev)) |>
  filter(!is.na(ribbonbark)) |>
  filter(!is.na(stringybark))
summary(g)
rm(g1, g2, g3,
   # g4,
   g5, g6, g7, g8, g9, g10, g10.1, g10.2, g10.3, g11, g12, g13, g14,
   # g15,
   g16)

g$winddiff.bom = cos((g$aspect - g$winddir) * pi / 180)
nrow(g)
## 1,690

# prefire 180 ####
setwd("E:/chapter3/GEDI_FESM")
gref = st_read("ch3_isochrons_prefire180.gpkg")
st_geometry(gref) = NULL
gref = gref |>
  dplyr::select(ID,
                maxtemp:maxwd,
                ffdi_final) |>
  unique()
length(unique(gref$ID)) == nrow(gref)
## TRUE
gref = inner_join(g, gref)
gref$winddiff.iso = cos((gref$aspect - gref$maxwd) * pi / 180)

gref$ffdi_cat = NA
gref$ffdi_cat[gref$ffdi_final <= 12] = "one"
gref$ffdi_cat[gref$ffdi_final > 12 & gref$ffdi_final <= 25] = "two"
gref$ffdi_cat[gref$ffdi_final > 25 & gref$ffdi_final <= 49] = "three"
gref$ffdi_cat[gref$ffdi_final > 49 & gref$ffdi_final <= 74] = "four"
gref$ffdi_cat[gref$ffdi_final > 74 & gref$ffdi_final <= 99] = "four"
gref$ffdi_cat[gref$ffdi_final > 99] = "four"
gref$ffdi_cat = factor(gref$ffdi_cat, levels = c("one",
                                           "two",
                                           "three",
                                           "four"))

summary(gref)
nrow(gref)
## 1,690
setwd("E:/chapter3/for GAMs")
st_write(gref, "ch3_forGAMs_poly_prefire180_final_redo_2-15.gpkg", delete_dsn = T)

