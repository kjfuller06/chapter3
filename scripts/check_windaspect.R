library(raster)
library(sf)
library(tmap)

# grab example data
setwd("E:/chapter3/GEDI_FESM")
gedi = st_read("ch3_FESMandfhist_forpoly.gpkg")
test = gedi[1,]
test = st_buffer(test, dist = 2000)

# convert slope facing to north
setwd("D:/chapter1/data")
aspect = raster("proj_dem_aspect_30m.tif")
slope = raster("proj_dem_slope_30m.tif")
alt = raster("proj_dem_s.tif")

test.asp = crop(aspect, test)
slope.asp = crop(slope, test)
alt.asp = crop(alt, test)

# how does subtracting angles work out?
hill = hillShade(test.asp*(pi/180), slope.asp, 40, 270)
north = cos(pi*test.asp / 180)
acos(north)
check.asp = pi*test.asp / 180
acos(cos(check.asp))
check.asp
acos(cos(3))
acos(cos(4))
## NOTE: cos cannot by reversed above the value of pi
abs = abs(north)

test.asp15 = test.asp - 15
hill15 = hillShade(test.asp15*(pi/180), slope.asp, 40, 270)
north15 = cos(test.asp15 * pi / 180)
abs15 = abs(north15)

test.asp90 = test.asp - 90
hill90 = hillShade(test.asp90*(pi/180), slope.asp, 40, 270)
north90 = cos(test.asp90 * pi / 180)
abs90 = abs(north90)
abseast90 = abs(sin(test.asp90 * pi / 180))
abswest90 = abs(sin((test.asp90 - 180) * pi / 180))

tm_shape(hill) + tm_raster(palette=grey(0:100/100)) + tm_shape(alt.asp) + tm_raster(palette=rainbow(25), alpha=0.35)
tm_shape(hill15) + tm_raster(palette=grey(0:100/100)) + tm_shape(alt.asp) + tm_raster(palette=rainbow(25), alpha=0.35)
tm_shape(hill90) + tm_raster(palette=grey(0:100/100)) + tm_shape(alt.asp) + tm_raster(palette=rainbow(25), alpha=0.35)

tm_shape(north) + tm_raster(palette=grey(0:100/100)) + tm_shape(alt.asp) + tm_raster(palette=rainbow(25), alpha=0.35)
tm_shape(north15) + tm_raster(palette=grey(0:100/100)) + tm_shape(alt.asp) + tm_raster(palette=rainbow(25), alpha=0.35)
tm_shape(north90) + tm_raster(palette=grey(0:100/100)) + tm_shape(alt.asp) + tm_raster(palette=rainbow(25), alpha=0.35)

subtract.test = abs - abseast90
tm_shape(abs(subtract.test)) + tm_raster()
## all zeros

subtract.test = abseast90 - abswest90
tm_shape(abs(subtract.test)) + tm_raster()
## all zeros

# can I subtract northness directly or do I need to subtract angles first?
cos((test.asp - 90) * pi/180)
north - north90
## no, I have to subtract the angle first

