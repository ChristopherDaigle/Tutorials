# linear regression models

library(MASS)
library(ISLR)

head(Boston) # Data about 506 neighborhood around Boston

# medv: median housing value
# rm: average number of rooms per house
# age: average age of houses
# lstat: percent of households with low socioeconomic status

lm.fit <- lm(medv~lstat, data=Boston)
summary(lm.fit)
names(lm.fit)

newX <- data.frame(lstat=c(5,10,15))
predict(object=lm.fit, newdata=newX)
predict(object=lm.fit, newdata=newX, interval="confidence")
predict(lm.fit, newdata=newX, interval="prediction")

plot(Boston$lstat,Boston$medv, pch=20)
abline(lm.fit,col="red",lwd=3, lty=5)

par(mfrow=c(2,2))
plot(lm.fit)
par(mfrow=c(1,1))

# 4. Residuals vs Leverage
# leverage point: an observation that has a value of x that is far away from the mean of x. 
# Influential observations: An influential observation is defined as an observation that changes the slope of the line. 
# Cook's distance", which is a measure of the influence of each observation on the regression coefficients.

# Multiple Linear Regression
lm.fit <- lm(medv~lstat+age, data=Boston)
summary(lm.fit)

# Interaction Terms
lm.fit1 <- lm(medv~lstat*age, data=Boston)
summary(lm.fit1)  # How to interpret the result? As a house is newer, the negative effect of lstat is more.
                  # As lstat is lower, the age of house affects the price more negatively. 
lm.fit2 <- lm(medv~lstat+age+lstat:age, data=Boston)
summary(lm.fit2)

predict(object=lm.fit1,newdata=data.frame(lstat=c(5,10,15),age=c(5,10,20)), interval="prediction")

x1 <- seq(min(Boston$lstat),max(Boston$lstat),length=30)
x2 <- seq(min(Boston$age),max(Boston$age),length=30)
yhat <- outer(x1,x2,function(x1,x2){predict(object=lm.fit1,newdata=data.frame(lstat=x1,age=x2))})

surface <- persp(x1,x2,yhat,phi=0, theta=0,col = terrain.colors(10),zlim=c(min(yhat)-10,max(Boston$medv)))
xy.list = trans3d(x=Boston$lstat, y=Boston$age, z=Boston$medv, pmat=surface)
points(xy.list, pch=20, col=1)

# Polynomial model
Boston$lstat2 <- (Boston$lstat)^2
lm.fit3 <- lm(medv~lstat+lstat2, data=Boston)
lm.fit4 <- lm(medv~lstat+I(lstat^2), data=Boston)
lm.fit5 <- lm(medv~poly(lstat,3), data=Boston)
summary(lm.fit5)
w = Boston$lstat + Boston$lstat2

# Exercise 1.
# Plot the fitted y with the prediction interval based on the quadratic regression model: lm.fit3 
plot(Boston$lstat, Boston$medv, ylim = c(0,50))
x <- seq(min(Boston$lstat)+1, max(Boston$lstat), length = 30)
x2 <- x^2
yhat <- predict(lm.fit3,newdata=data.frame(lstat = x, lstat2 = x2), interval="prediction")
lines(x, yhat[,1], lwd = 3)
lines(x, yhat[,2], lty = 3, lwd = 2, col = 'red')
lines(x, yhat[,3], lty = 3, lwd = 2, col = 'red')

#  Exercise 2 
# (1) Download the housing dataset from https://www.kaggle.com/harlfoxem/housesalesprediction
#     and run a regression to predict housing prices. 

# (2) Build a model to predict the housing price given characteristics of a house in the dataset.
#     Consider the following predictors:
#    
#     season, sqft_living, yr_built, interaction of sqft_living yr_built, and waterfront

#     Create a variable "season" which equals 
#     "Winter" if a house was sold in Jan, Feb, Mar, Dec.
#     "Spring" if it was sold in Apr, May, Jun
#     "Summer" if it was sold in Jul, Aug
#     "Fall" if it was sold in Sep, Oct, Nov.
#     Do you find any seasonality in housing price?

# (3) Do you find any nonlinearity or heteroskedasticity? 
#     What is the problem if the error term is heteroskedastic?
#     How can you address these problems (if you have here)?

# (4) Conduct an F test for the following hypotheses.
#     H0: there is no seasonality on the housing price. H1: H0 is not true.

# (5) Pridict the housing price when season = spring, sqft_living=2500, yr_built=2000, waterfront=0.


#  Exercise 3
#  Simulate time series data and see that conventional tests may not work in the time series. 
#  e.g. GDP growth, stock price, a firm's revenue.... Marketing
#
# Note that random sampling ensures iid, but in time series, it is not randomly
# sampled and it is dependent (serial correlation or autocorrelation) and that
# can be a problem in inference because the standard error will be invalid and
# thus the t-statistic will be invalid and then the p-value will be invalid

set.seed(102)

Sales<- rep(NA,100)
Online <- rep(NA,100)
e <- rep(NA,100)

Online[1] <- 2*rnorm(1) #initial value of x
e[1] <- rnorm(1) # initial value of e
b0 <- 1
b1 <- 0
Sales[1] <- b0 + b1*Online[1] + e[1] #true DGP of our simulation
rho1 <- 0.7
rho2 <- 0.7

for (t in 2:100) {
  Online[t] <- rho1*Online[t-1] + rnorm(1) # online budget is highly correlated with yesterday's budget
  e[t] <- rho2*e[t-1] + rnorm(1) # error term is also dependent on previous, implies serially correlation
  Sales[t] <- b0 + b1*Online[t] + e[t] # this is our 
}

linear.fit <- lm(Sales~Online)
summary(linear.fit) # We reject the null hypothesis based on the significance level of the p-value
confint(linear.fit)[2,] # again, we can see the value is statistically significant from zero

# We can see that we reject H0: b1=0. So, we conclude that TV advertisement is associated with Sales.
# But, this is a mistake. The true b1=0. If our inference procedure is valid, when we test 100 times, 
# our mistake should be around 5 times.
# Using simulations, count how many times falsely reject H0, when we replicate the procedure above 1000 times.


################################################################################################################

## K nearest neighbors method
rm(list=ls())

set.seed(3)

X <- runif(n=500,min=0,max=2*pi)
e <- rnorm(n=500,mean=0,sd=0.5)
f <- function(x){
  2*sin(3*x)*cos(2*x)*0.5*x
}

Y <- f(X) + e

par(mfrow=c(1,1))
plot(X,Y)
curve(f,from=0,to=2*pi,add=TRUE)

knn = function(x0,X,Y,K){
  x0 <- matrix(rep(x0,length(Y)),byrow=TRUE)
  X <- matrix(X)
  distance <- rowSums((x0-X)^2)
  rank <- order(distance)
  Y_K <- Y[rank][1:K]
  mean(Y_K)
}

knn(pi,X,Y,10)

# Exercise: 
# 1. Using for-loop, obtain k-nearest neighbor estimate when x=seq(0,2*pi,length=30)
# and draw the line. Do this with different K=1,5,10
# 2. Using for-loop, calculate the bias, variance and MSE of this estimator at X=pi with different K's.
# Draw this three lines in a 

x <- seq(0.5,2*pi-0.5,length=30)
fhat <- matrix(rep(NA,120),30,4)
for (j in 1:4){
  K = 10*j-9
  for (i in 1:30){
    fhat[i,j] <- knn(x[i],X,Y,K)    
  }
}

lines(x,fhat[,1],col="red",lwd=2)
lines(x,fhat[,2],col="purple",lwd=2, lty=3)
lines(x,fhat[,3],col="black",lwd=2, lty=10)
lines(x,fhat[,4],col="blue",lwd=2)

rm(list=ls())

B <- matrix(rep(NA,2500),1000,10)
V <- matrix(rep(NA,2500),1000,10)

for (i in 1:1000){
  X <- runif(n=100,min=0,max=4*pi)
  e <- rnorm(n=100,mean=0,sd=0.5)
  Y <- f(X) + e
  for (j in 1:10){
    K <- 3*j-2
    B[i,j] <- knn(pi,X,Y,K) - f(pi)
    V[i,j] <- knn(pi,X,Y,K)
  }
}
Bias <- colMeans(B)
Bias2 <- Bias^2
Var <- c(var(V[,1]),var(V[,2]),var(V[,3]),var(V[,4]),var(V[,5]),
         var(V[,6]),var(V[,7]),var(V[,8]),var(V[,9]),var(V[,10]))
MSE <- Bias2 + Var

K_vec <- 3*(1:10)-2  
plot(K_vec,MSE, type="l", lty=1, lwd=3, ylim=c(0,0.6))
points(K_vec,Bias2, col="blue", type="l", lty=5, lwd=3)
points(K_vec,Var,col="red", type="l", lty=10, lwd=3)

# Exercise 3. Download "Auto.csv" from http://www-bcf.usc.edu/~gareth/ISL/data.html
# and estimate the regression of mpg on horsepower using the KNN method. Compare the result with
# linear regression estimate.

setwd("C:/Users/msk17004/Dropbox/UConn/BigData")
Auto <- read.table("Auto.data",header=TRUE)
head(Auto)
str(Auto)

plot(Auto$weight,Auto$mpg)
linear.fit <- lm(mpg~weight,data=Auto)
abline(linear.fit)
a <- 70
K <- 50
x_vec <- seq(min(Auto$weight)+500,max(Auto$weight)-500,length=a)
fhat <- rep(NA,a)
for (i in 1:a) {
  fhat[i] <- knn(x_vec[i],Auto$weight,Auto$mpg,K)
}
lines(x_vec,fhat,lwd=3)
