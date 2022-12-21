library(raster)
library(sf)
library(tidyverse)
library(geojsonsf)
library(rjson)
library(rgeos)
library(terra)
library(tmap)

# load files ####
# setwd("/glade/scratch/kjfuller/data")
setwd("E:/chapter3/isochrons")
iso = st_read("isochrons_8days.gpkg")
targetcrs = st_crs(iso)
iso$poly_sD = as.POSIXct(iso$poly_sD)
iso = iso |> 
  dplyr::select(ID, poly_sD)

setwd("E:/chapter3/roadways/")
access = st_read("access_lines_restricted10km.gpkg")
access$startdate = as.Date(access$startdate)

# removed 30 dates ####
dates = sort(unique(access$startdate))
dates_perm = dates
df = data.frame(index = c(1:length(dates)), date = dates)

names(access)[1] = "date"
access = access |> 
  left_join(df)

# select all features that were added during the selected times- only calculate distances for any new features, to save computation time
access_temp = access |> 
  filter(date <= dates[1])

iso_r = raster(ext = extent(iso), res = 100, crs = crs(iso))

# limit features to only those features which fall within 10km of the extent of the sampling area, to save computation time
iso_ext = st_as_sf(as.polygons(ext(iso_r)))
st_crs(iso_ext) = targetcrs
iso_ext = st_buffer(iso_ext, dist = 10000)
a_filter = access_temp[iso_ext,]

l = list()
a = 1
l[[a]] = a_filter
a = a + 1
# now run for all other intervals
for(i in c(1:(length(dates_perm) - 1))){
  print(paste0("Iteration ", i , ": check dates ", dates[i], " to ", dates[i + 1]))
  # check if any fires occurred between the dates when features were added
  iso_temp = iso |> 
    filter(poly_sD > dates[i] & poly_sD <= dates[i + 1])
  
  if(nrow(iso_temp) == 0){
    df$date[(i + 1)] = NA
    print("nrow(iso_temp) = 0")
  } else {
    # select all fires that occurred *after* all selected features were added (all fires for which the features were relevant)
    # select access features by date (will need to take the min of all relevant rasters, running through each date of "access" and grabbing all previous rasters, as each date represents new, additional access features to include in potential minimum distance)
    iso_temp = iso |> 
      filter(poly_sD >= dates[i + 1])
    
    # select all features that were added during the selected times- only calculate distances for any new features, to save computation time
    access_temp = access |> 
      filter(date > dates[i] & date <= dates[i + 1])
    
    iso_r = raster(ext = extent(iso_temp), res = 100, crs = crs(iso_temp))
    
    # limit features to only those features which fall within 10km of the extent of the sampling area, to save computation time
    iso_ext = st_as_sf(as.polygons(ext(iso_r)))
    st_crs(iso_ext) = targetcrs
    iso_ext = st_buffer(iso_ext, dist = 10000)
    a_filter = access_temp[iso_ext,]
    
    if(nrow(a_filter) < 1){
      print("nrow(a_filter) = 0")
    } else {
      l[[a]] = a_filter
      a = a + 1
    }
  }
}
df
l = bind_rows(l)
dates = unique(l$date)
dates = dates[dates %in% df$date]
keep = data.frame(index = c(1:length(dates)), date = sort(dates))
keep

keep$lat = 0
keep$lon = 0
keep = st_as_sf(keep, coords = c("lon", "lat"), crs = 4326)
st_write(keep, "access_keepdates.gpkg", delete_dsn = T)

st_write(l, "access_linestrings_datefiltered.gpkg", delete_dsn = T)

# check dates ####
setwd("E:/chapter3/isochrons")
iso = st_read("isochrons_8days.gpkg")
targetcrs = st_crs(iso)
iso$poly_sD = as.POSIXct(iso$poly_sD)
iso = iso |> 
  dplyr::select(ID, poly_sD)

setwd("E:/chapter3/roadways/")
keep = st_read("access_keepdates.gpkg")
st_geometry(keep) = NULL

access = st_read("access_linestrings_datefiltered.gpkg")
access$ID = c(1:nrow(access))
minD = min(access$date)

tmap_mode("view")

x = 2
dat = keep$date[x]

a_temp = access |> 
  filter(date == dat)

iso_temp = iso |> 
  filter(poly_sD >= dat)

iso_r = raster(ext = extent(iso_temp), res = 100, crs = crs(iso_temp))
iso_ext = st_as_sf(as.polygons(ext(iso_r)))
st_crs(iso_ext) = targetcrs

# tm_shape(iso_ext) + tm_borders() + tm_shape(a_temp) + tm_lines()

# summary(a_temp$ID)
a_fix = a_temp |> 
  filter(ID > 901311)

# tm_shape(iso_ext) + tm_borders() + tm_shape(a_fix) + tm_lines()

a_fix$date = minD
access = access |> 
  filter(!ID %in% a_fix$ID)
access = rbind(access, a_fix)

x = 11
dat = keep$date[x]

a_temp = access |> 
  filter(date == dat)

iso_temp = iso |> 
  filter(poly_sD >= dat)

iso_r = raster(ext = extent(iso_temp), res = 100, crs = crs(iso_temp))
iso_ext = st_as_sf(as.polygons(ext(iso_r)))
st_crs(iso_ext) = targetcrs

# tm_shape(iso_ext) + tm_borders() + tm_shape(a_temp) + tm_lines()

# summary(a_temp$ID)
a_fix = a_temp |> 
  filter(ID != 905200 & ID != 905013)

# tm_shape(iso_ext) + tm_borders() + tm_shape(a_fix) + tm_lines()

a_fix$date = minD
access = access |> 
  filter(!ID %in% a_fix$ID)
access = rbind(access, a_fix)

x = 12
dat = keep$date[x]

a_temp = access |> 
  filter(date == dat)

iso_temp = iso |> 
  filter(poly_sD >= dat)

iso_r = raster(ext = extent(iso_temp), res = 100, crs = crs(iso_temp))
iso_ext = st_as_sf(as.polygons(ext(iso_r)))
st_crs(iso_ext) = targetcrs

# tm_shape(iso_ext) + tm_borders() + tm_shape(a_temp) + tm_lines()

a_temp$date = minD
access = access |> 
  filter(!ID %in% a_temp$ID)
access = rbind(access, a_temp)

x = 13
dat = keep$date[x]

a_temp = access |> 
  filter(date == dat)

iso_temp = iso |> 
  filter(poly_sD >= dat)

iso_r = raster(ext = extent(iso_temp), res = 100, crs = crs(iso_temp))
iso_ext = st_as_sf(as.polygons(ext(iso_r)))
st_crs(iso_ext) = targetcrs

tm_shape(iso_ext) + tm_borders() + tm_shape(a_temp) + tm_lines()

a_temp$date = minD
access = access |> 
  filter(!ID %in% a_temp$ID)
access = rbind(access, a_temp)
