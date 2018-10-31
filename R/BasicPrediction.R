# Linear Regression models

library(MASS)
library(ISLR)

head(Boston) # data about 506 neighborhoods around Boston

# medv: median housing value
# rm: average number of rooms per house
# ageL averag age of houses
# lstat: percent of households with low ecionomic status

lm.fit <- lm(medv~lstat, data = Boston)
summary(lm.fit)
names(lm.fit)

newX <- data.frame(lstat = c(5,10,15)) # 5, 10, and 15%
predict(object = lm.fit, newdata = newX)
predict(object = lm.fit, newdata = newX, interval = 'confidence') # conditional mean of y given x
# E(hat)[Y|X] = Y(hat) , CI = [Y(hat) - 1.96*se(Y(hat), Y(hat) + 1.96*se(Y(hat))]
# PI = [Y(hat) - 1.96* sqrt(var(Y(hat) + var(epsilon))), Y(hat) + 1.96* sqrt(var(Y(hat) + var(epsilon)))]
predict(lm.fit, newdata = newX, interval = 'prediction')

plot(Boston$lstat, Boston$medv, pch = 20)
abline(lm.fit, col = 'red', lwd= 3, lty = 5)

par(mfrow= c(2,2))
plot(lm.fit)
par(mfrow = c(1,1))

# If data is heteroskedastic, you can use GLS, but in economics, we typically use White Standard Error
# Heterosked does not effect consistency of the estimator, but can cause problems in inference
