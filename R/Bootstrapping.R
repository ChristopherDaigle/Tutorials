#' ___
#' Chris Daigle
#' 
#' Bootstrap
#' 
#' 
#' 
#' ___
#' 
rm(list = ls())
library(ISLR)
library(boot)
# Estimating the accuracy of a statistic of interest ####

head(Portfolio)

alpha.fn <- function(data, index) {
  X <- data[index, 1]
  Y <- data[index, 2]
  alpha <- (var(Y) - cov(X,Y)) / (var(X)+var(Y)-2*cov(X,Y))
  return(alpha)
}

alpha.fn(Portfolio, 1:nrow(Portfolio))

bootstrap <- boot(data = Portfolio, statistic = alpha.fn, R = 100)
bootstrap

bootstrap$t0
bootstrap$t # Every t-stat estimate from R 
# Estimating the accuracy of a linear regression model
lm.cof <- function(data, index) {
  data.1 <- data[index,]
  names(data.1) <- letters[1:ncol(data.1)]
  coef(lm(a~., data = data.1))
}

Auto.boot <- Auto[, c(1, 4, 5)]
boot(data = Auto.boot, statistic = lm.cof, R = 100)
summary(lm(mpg ~ horsepower + weight, data = Auto.boot))

# Sample Model ####
plot(mtcars$wt, mtcars$mpg)
x <- mtcars$wt
y <- mtcars$mpg

reg <- lm(y ~ x)
abline(reg)

bHat <- reg$coefficients[2]
aHat <- reg$coefficients[1]
n <- nrow(mtcars)
num <- sum(((x - mean(x)) ^ 2) * (reg$residuals^2))/n
den <- sum((x-mean(x))^2)/(n-1)
se <- sqrt(num)/den

t <- sqrt(n) * bHat / se

# Pairwise Bootstrap ####
B <- 999
bPair <- rep(NA, B)
tPair <- rep(NA, B)

for (b in 1:B) {
  
  index <- sample(1:nrow(mtcars), size = n, replace = TRUE)
  xPair <- mtcars$wt[index]
  yPair <- mtcars$mpg[index]
  
  regPair <- lm(yPair ~ xPair)
  bPair[b] <- regPair$coefficients[2]
  
  numPair <- sum(((xPair - mean(xPair)) ^ 2) * (regPair$residuals ^ 2)) / n
  denPair <- sum((xPair - mean(xPair)) ^ 2) / (n - 1)
  sePair <- sqrt(numPair) / denPair
  
  tPair[b] <- sqrt(n) * (bPair[b] - bHat) / sePair
}

tPair <- sort(tPair)

hist(tPair, breaks = 30, probability = TRUE, col = "grey", 
     main = "Distribution of t* Under Pairwise Bootstrap")
lines(density(tPair), col = "red", lwd = 3)
critPair <- c(tPair[25], tPair[975])
abline(v = c(critPair, bHat), col = c("blue"), lty = c(2, 2, 1), lwd = 3)
cat(paste("The coefficient of intrest is", round(bHat, 3), 
          "and the t-statistic is", round(t, 3), "."))
cat(paste("The 95% critical values are", round(critPair[1], 3), 
          "and", round(critPair[2], 3), "."))

# Residual Bootstrap ####
bRes <- rep(NA, B)
tRes <- rep(NA, B)

for (b in 1:B) {
  index <- sample(1:nrow(mtcars), size = n, replace = TRUE)
  xRes <- x
  uRes <- reg$residuals[index]
  yRes <- aHat + bHat * xRes + uRes
  
  regRes <- lm(yRes ~ xRes)
  bRes[b] <- regRes$coefficients[2]
  
  
  numRes <- sum(((xRes - mean(xRes)) ^ 2) * (regRes$residuals ^ 2)) / n
  denRes <- sum((xRes - mean(xRes)) ^ 2) / (n - 1)
  seRes <- sqrt(numRes) / denRes
  
  tRes[b] <- sqrt(n) * (bRes[b] - bHat) / seRes
}

tRes <- sort(tRes)

hist(tRes, breaks = 30, probability = TRUE, col= "grey", 
     main = "Distribution of t* Under Residual Bootstrap")
lines(density(tRes), col = "red", lwd = 3)
critRes <- c(tRes[25], tRes[975])
abline(v = c(critRes, bHat), col = c("blue"), lty = c(2, 2, 1), lwd = 3)
cat(paste("The coefficient of intrest is", round(bHat, 3), 
          "and the t-statistic is", round(t, 3), "."))
cat(paste("The 95% critical values are", round(critRes[1], 3), 
          "and", round(critRes[2], 3), "."))

# Wild Bootstrap ####
bWild <- rep(NA, B)
tWild <- rep(NA, B)

for (b in 1:B) {
  sig <- sample(c(-1, 1), size = n, replace = TRUE)
  xWild <- x
  uWild <- reg$residuals*sig
  yWild <- aHat + bHat * xWild + uWild
  
  regWild <- lm(yWild ~ xWild)
  bWild[b] <- regWild$coefficients[2]
  
  numWild <- sum(((xWild - mean(xWild)) ^ 2) * (regWild$residuals ^ 2)) / n
  denWild <- sum((xWild - mean(xWild)) ^ 2) / (n - 1)
  seWild <- sqrt(numWild)/ denWild
  
  tWild[b] <- (sqrt(n) * (bWild[b] - bHat)) / seWild
}

tWild <- sort(tWild)
hist(tWild, breaks = 30, probability = TRUE, col = "grey", 
     main = "Dist. of t* Under Wild Bootstrap", xlim = c(-5, 5))
lines(density(tWild), col = "red", lwd = 3)
critWild <- c(tWild[25], tWild[975])
abline(v = c(critWild, bHat), col = c("blue"), lty = c(2, 2, 1), lwd = 3)
cat(paste("The coefficient of intrest is", round(bHat, 3), 
          "and the t-statistic is", round(t, 3), "."))
cat(paste("The 95% critical values are", round(critWild[1], 3), 
          "and", round(critWild[2], 3), "."))

