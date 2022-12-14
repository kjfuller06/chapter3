library(tidyverse)

# slope = c(1, 10, 30, 60, 90, 95, 130, 150, 180, 240, 250, 300, 330, 360)
# angle = rep(90, length(slope))
# 
# wind = slope - angle
# wind[wind < 0] = wind[wind < 0] + 360
# 
# diff = slope - wind
# diff[diff < 0] = diff[diff < 0] + 360
# 
# diff.north = slope.north - wind.north
# diff.east = slope.east - wind.east
# diff = data.frame(diff.north = diff.north,
#                   diff.east = diff.east)
# diff$diff.total = abs(diff$diff.north) + abs(diff$diff.east)
# 
# plot(diff$diff.total ~ angle)

angle = seq(1, 360, by = 1)
slope = 90

wind = slope - angle
wind[wind < 0] = wind[wind < 0] + 360

diff = slope - wind
diff[diff < 0] = diff[diff < 0] + 360
northness = cos(diff * pi / 180)
# eastness = sin(diff * pi / 180)

plot(northness ~ angle)
# plot(eastness ~ angle)

df = data.frame(difference = angle, index = northness)
ggplot(df, aes(y = index, x = difference)) +
  geom_point() +
  scale_x_continuous(name = "Difference in Aspect Between Slope and Wind", breaks = c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330, 360)) +
  ylab("Difference Index")

slope = c(1, 10, 30, 60, 90, 95, 130, 150, 180, 240, 250, 300, 330, 360)
angle = rep(180, length(slope))

wind = slope - angle
wind[wind < 0] = wind[wind < 0] + 360

diff = slope - wind
diff[diff < 0] = diff[diff < 0] + 360
northness = cos(diff * pi / 180)
eastness = sin(diff * pi / 180)

plot(northness ~ angle, ylim = c(-1, 1))
