library(mgcv)
set.seed(6)
## simulate some data from a three class model
n <- 1000
f1 <- function(x) sin(3*pi*x)*exp(-x)
f2 <- function(x) x^3
f3 <- function(x) .5*exp(-x^2)-.2
f4 <- function(x) 1
x1 <- runif(n);x2 <- runif(n); x3 = runif(n); x4 = runif(n)

eta1 <- 2*(f1(x1) + f2(x2))-.5
eta2 <- 2*(f3(x1) + f4(x2))-1
p <- exp(cbind(0,eta1,eta2))
p <- p/rowSums(p) ## prob. of each category 
cp <- t(apply(p,1,cumsum)) ## cumulative prob.
## simulate multinomial response with these probabilities
## see also ?rmultinom
y <- apply(cp,1,function(x) min(which(x>runif(1))))-1
## plot simulated data...
# plot(x1,x2,col=y+3)
y[1:500] = y[1:500] + 1
# y[1:100] = y[1:100] + 1
# y[101:200] = y[101:200] + 2

## now fit the model...
b <- gam(list(y~s(x1) + s(x3) + s(x2) + s(x4),
              ~s(x1) + s(x3) + s(x2) + s(x4),
              ~s(x1) + s(x3) + s(x2) + s(x4)),
         family=multinom(K=3))
## K = number of classes - 1; can add lots of predictors