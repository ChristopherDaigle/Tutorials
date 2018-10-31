# K Nearest NEighbors
set.seed(3)
rm(list = ls())

X <- runif(n = 500, min =0, max = 2* pi)
e <- rnorm(n = 500, mean = 0, sd = 0.5)

f <- function(x) {
  2*sin(3*x) * cos(2*x)*0.5*x
}

Y <- f(X) + e

par(mfrow = c(1,1))
plot(X,Y)
curve(f, from = 0, to = 2*pi, add = TRUE, col = 'green')

knn <- function(x0, X, Y, K) {
  x0 <- matrix(rep(x0, length(Y)), byrow = TRUE)
  X <- matrix(X)
  distance <- rowSums((x0 - X)^2)
  rank <- order(distance)
  Y_K <- Y[rank][1:K]
  mean(Y_K)
}

knn(3,X,Y,5)
f(3)

# Using for-loop, ovtain k nearest neighbors

x <- seq(0.5, 2*pi - 0.5, length = 30)
fhat <- matrix(rep(NA,120), 30, 4)
for (j in 1:4) {
  K = 10*j - 9
  for (i in 1:30) {
    fhat[i,j] <- knn(x[i], X, Y, K)
  }
}

lines(x,fhat[,1], col = 'red', lwd= 2)
lines(x,fhat[,2], col = 'purple', lwd= 2)
lines(x,fhat[,3], col = 'black', lwd= 2, lty = 10)
lines(x,fhat[,4], col = 'blue', lwd= 2)

B <- matrix(rep(NA,10000), 1000, 10)
V <- matrix(rep(NA,10000), 1000, 10)

for (i in 1:1000) {
  X <- runif(n = 100, min = 0, max = 4*pi)
  e <- rnorm(n = 100, mean = 0, sd = 0.5)
  Y <- f(X) + e
  for (j in 1:10) {
    K <- 3*j-2
    B[i,j] <- knn(pi, X, Y, K) - f(pi)
    V[i,j] <- knn(pi, X, Y, K)
  }
}

Bias <- colMeans(B)
Bias2 <- Bias ^ 2
Var <- c(var(V[,1]), var(V[,2]), var(V[,3]), var(V[,4]), var(V[,5]), var(V[,6]), var(V[,7]), var(V[,8]), var(V[,9]), var(V[,10]))
MSE <- Bias2 + Var

KVec <- 3*(1:10)-2
plot(KVec, Bias2, col = 'blue', type = 'l', lty = 5, lwd = 3, ylim = c(0,0.8))
points(KVec, Var, col = 'red', type = 'l', lty = 10, lwd = 3)
points(KVec, MSE, type = 'l', lty = 1, lwd = 3)

