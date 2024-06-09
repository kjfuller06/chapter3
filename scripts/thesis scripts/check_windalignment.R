library(tidyverse)

# wind-slope correction factors ####
# Sharples 2008- eq 30, recommended in the Discussion section
# U`ws = ||w|| sin(θw − γs)ˆt + (||w|| cos(θw − γs) + 150.98(tan γs)^1.2)û

## U`ws = slope-corrected effective wind speed
## ||w|| = wind speed
w = windspeed
## θw = wind direction
thetaw = winddir
## γs = slope in degrees
ys = slope
## ˆt = unit vector in the across-slope (t) direction
## ˆu = unit vector in the upslope-slope (u) direction

wind.slope = windspeed* sin(wind.direction - slope) + 
  windspeed* cos(wind.direction - slope) + 150.98*(tan(slope)^1.2)

## is slope an error in the sin() and cos() terms??
## ^ yes, so the corrected formula is:

wind.slope = windspeed* sin(wind.direction - aspect) + 
  windspeed* cos(wind.direction - aspect) + 150.98*(tan(slope)^1.2)

# original, equation 6:
# R(w, γs) = Rw * sin(θw −γa)ˆt + Rw * cos(θw −γa) * exp(0.069γs)û

## magnitude of the slope-corrected rate of spread for equation 6
# R(w, γs) = Rw * sqrt(sin2(θw − γa) + cos2(θw − γa) exp(0.138γs))

## so, the equivalent for equation 30 would be:
sqrt(windspeed^2 * sin(wind.direction - aspect)^2 + (windspeed* cos(wind.direction - aspect) + 150.98*tan(slope)^1.2)^2)

slope.exa = seq(0, 40, by = 1)
r0 = exp(3.533*(tan(slope.exa * pi/180))^1.2)
r1 = exp(3.533*(tan(slope.exa))^1.2)
plot(r0 ~ slope.exa,
     xlim = c(0, 40),
     ylim = c(0, 20))
## yep, I want to multiply slope by pi/180

grid1 = expand.grid(aspect = seq(0, 360, by = 10),
                    slope = seq(0, 40, by = 10),
                    windspeed = seq(0, 40, by = 10),
                    winddir = seq(0, 360, by = 10))
grid1 = expand.grid(aspect = 0,
                    slope = -10,
                    windspeed = 10,
                    winddir = seq(0, 360, by = 10))
nrow(grid1)
## 280,645

# record slope aspects that are negative relative to spread direction
vect = grid1$aspect > 90 & grid1$aspect < 270
grid1$slope[vect] = -grid1$slope[vect]

# calculate the net effective wind vector in the t and u directions
# w.s.speed = grid1$windspeed* sin(grid1$winddir*pi/180 - grid1$slope*pi/180) + grid1$windspeed* cos(grid1$winddir*pi/180 - grid1$slope*pi/180)
# slopecorrection = 150.98*tan(abs(grid1$slope)*pi/180)^1.2
# slopecorrection[vect] = -slopecorrection[vect]
# grid1$w.s.speed = w.s.speed + slopecorrection

w.s.speed = grid1$windspeed* sin(grid1$winddir*pi/180 - grid1$aspect*pi/180) + grid1$windspeed* cos(grid1$winddir*pi/180 - grid1$aspect*pi/180)
slopecorrection = 150.98*tan(abs(grid1$slope)*pi/180)^1.2
slopecorrection[vect] = -slopecorrection[vect]
grid1$w.s.speed = w.s.speed + slopecorrection

w.s.speed = sqrt(grid1$windspeed^2 * sin(grid1$winddir*pi/180 - grid1$aspect*pi/180)^2 + (grid1$windspeed* cos(grid1$winddir*pi/180 - grid1$aspect*pi/180) + 150.98*tan(abs(grid1$slope)*pi/180)^1.2)^2)
# slopecorrection[vect] = -slopecorrection[vect]
grid1$w.s.speed.cor = w.s.speed

# # windspeed* sqrt(sin(wind.direction - aspect)^2 + cos(wind.direction - aspect)^2 + (150.98*(tan(slope)^1.2))^2)
# w.s.speed.t = sin(grid1$winddir*pi/180 - grid1$aspect*pi/180)^2
# w.s.speed.u = cos(grid1$winddir*pi/180 - grid1$aspect*pi/180)^2
# slopecorrection = (150.98*tan(abs(grid1$slope)*pi/180)^1.2)^2
# slopecorrection[vect] = -slopecorrection[vect]
# w.s.speed.u = w.s.speed.u + slopecorrection
# grid1$w.s.speed.mag = grid1$windspeed*sqrt(w.s.speed.t + w.s.speed.u)
# ## doesn't work. All values equal

grid1$windspeed.cat = NA
grid1$windspeed.cat[grid1$windspeed <= 10] = "low"
grid1$windspeed.cat[grid1$windspeed > 10 & grid1$windspeed < 30] = "moderate"
grid1$windspeed.cat[grid1$windspeed >= 30] = "high"
grid1$windspeed.cat = factor(grid1$windspeed.cat, levels = c("low",
                                                         "moderate",
                                                         "high"))

grid1$aspect.cat = NA
grid1$aspect.cat[grid1$aspect > 90 & grid1$aspect < 270] = "against"
grid1$aspect.cat[grid1$aspect <= 90 | grid1$aspect >= 270] = "with"

grid1$slope.cat = NA
grid1$slope.cat[grid1$slope >= 0] = "upslope"
grid1$slope.cat[grid1$slope < 0] = "downslope"

grid1$winddir.cat = NA
grid1$winddir.cat[grid1$winddir > 90 & grid1$winddir < 270] = "against"
grid1$winddir.cat[grid1$winddir <= 90 | grid1$winddir >= 270] = "with"

# ggplot(grid1, aes(x = slope, y = w.s.speed)) +
#   geom_point(aes(col = aspect.cat)) +
#   geom_smooth(method = "lm", aes(col = aspect.cat))
ggplot(grid1, aes(x = slope, y = w.s.speed.cor)) +
  geom_point(aes(col = winddir.cat)) +
  geom_smooth(method = "lm", aes(col = winddir.cat))

# ggplot(grid1, aes(x = winddir, y = w.s.speed)) +
#   geom_point(aes(col = windspeed)) +
#   scale_x_continuous(breaks = c(0, 90, 180, 270, 360)) + 
#   facet_wrap(facets = "windspeed.cat")
ggplot(grid1, aes(x = winddir, y = w.s.speed.cor)) +
  geom_point(aes(col = aspect)) +
  scale_x_continuous(breaks = c(0, 90, 180, 270, 360)) + 
  facet_wrap(facets = "windspeed.cat") +
  ylim(0, 35)

ggplot(grid1, aes(x = winddir, y = w.s.speed.cor)) +
  geom_point() +
  scale_x_continuous(breaks = c(0, 90, 180, 270, 360)) +
  xlab("Wind direction (degrees)") +
  ylab("Net effective wind vector") +
  ggtitle("Additive wind vector with slope of 10 degrees\nand wind speed of 10 kph")

ggplot(grid1, aes(x = winddir, y = w.s.speed.mag)) +
  geom_point() +
  scale_x_continuous(breaks = c(0, 90, 180, 270, 360)) +
  xlab("Wind direction (degrees)") +
  ylab("Net effective wind vector") +
  ggtitle("Additive wind vector with slope of 10 degrees\nand wind speed of 10 kph")

## double-check wind direction effect
## double-check wind and slope alignment, with modified slope equation

# ggplot(grid1, aes(x = windspeed, y = w.s.speed)) +
#   geom_point(aes(col = winddir.cat)) +
#   facet_wrap(facets = "winddir.cat")
ggplot(grid1, aes(x = windspeed, y = w.s.speed.cor)) +
  geom_point(aes(col = winddir.cat)) +
  facet_wrap(facets = "winddir.cat")

# other possible equations ####
# Sharples 2008- eq 6
# R(w, γs) = Rw sin(θw −γa)ˆt +Rw cos(θw −γa) exp(0.069γs)û 

## R(w, γs) = slope- and wind-induced rate of fire spread
## Rw = wind-induced rate of fire spread
## θw = wind direction
## γa = slope aspect
## ˆt = unit vector in the across-slope (t) direction
## γs = slope in degrees
## ˆu = unit vector in the upslope-slope (u) direction

# Sharples 2008- eq 8
# θR = γa + π/2 + tan^−1{cot(θw − γa) exp(0.069γs)}

## θR = slope-corrected rate of spread vector relative to cardinal axes
## γa = slope aspect
## θw = wind direction
## γs = slope in degrees
