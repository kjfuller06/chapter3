library(raster)
library(FNN)

r <- raster(ncol=36,nrow=18)
values(r) <- NA
r[500] <- 1
dist <- distance(r) 
plot(dist / 1000)


sigma<- function(v, r, p)
{
  V<- matrix(r^2, ncol=p, nrow=p)
  diag(V)<- 1
  V*v
}

# create a raster with NA's randomly assigned
set.seed(310366)
r1 = raster(ncol=20,nrow=12)
r1[] = 0
r1[sample(ncell(r1),10)]=rep(NA,10)
plot(r1)

# create a raster with values randomly assigned, surrounded by Na's
r2 = raster(ncol=200, nrow=120)
r2[] = NA
r2[sample(ncell(r2),20)] = 1
plot(r2)

# grab the center coordinates of cells- NA's from the first raster and non-NA's from the second
p1 = as.data.frame(r1,xy=TRUE)
p1 = p1[is.na(p1[,3]),1:2]

p2 = as.data.frame(r2, xy=TRUE)
p2 = p2[!is.na(p2[,3]),1:2]

# check it out
plot(r1, col="grey")
plot(r2, add=TRUE, col=c("white","black"),legend=FALSE)
points(p1$x, p1$y)
points(p2$x, p2$y, pch=3)

# calculate the distance between each set of points- returns 10 values corresponding to the 10 NA's from r1
dnear = knnx.dist(data = p2, query = p1, k=1)

# assign the values to the NA cells in the first raster
r1[is.na(r1)] = dnear[,1]
