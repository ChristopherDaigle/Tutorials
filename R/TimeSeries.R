#'
#' Time Series Forecasting Tutorial
#' 
#' 
rm(list = ls())
setwd(
  "~/Library/Mobile Documents/com~apple~CloudDocs/Education/UConn/Spring 2018/R/DataSets"
)

excaus <- read.csv('EXCAUS.csv', stringsAsFactors = FALSE)

head(excaus)
str(excaus)
excaus$DATE <- as.Date(excaus$DATE)
excaus$Time <- format(excaus$DATE, format = '%y/%m')
excaus <- excaus[, c(3, 2)]

excaus1 <- mean(excaus[excaus$Time >= 16 / 01, 2])

# Use ts() function
excaus <-
  ts(
    excaus$EXCAUS,
    start = c(2008, 1),
    end = c(2018, 4),
    frequency = 12
  )
str(excaus)
start(excaus)
end(excaus)
frequency(excaus)
summary(excaus)
excaus

plot(excaus,
     col = 'blue',
     lwd = 3,
     ylab = 'Exchange Rate')
abline(reg = lm(excaus ~ time(excaus)), lwd = 3)

# display yearly averages
plot(aggregate(excaus, FUN = mean))

# Box plot across months will give us a sense on seasonal effect
boxplot(excaus ~ cycle(excaus))

exuseu <- read.csv('EXUSEU.csv', stringsAsFactors = FALSE)
exuseu
exeuus <- (exuseu$DEXUSEU) ^ (-1)
exeuus <-
  ts(
    exeuus,
    start = c(2008, 4),
    end = c(2018, 3),
    frequency = 12
  )
plot(
  excaus,
  col = 'blue',
  lwd = 3,
  ylim = c(0.6, 1.5),
  ylab = 'Exchange Rate'
)
lines(exeuus,
      col = 'red',
      lty = 2,
      lwd = 3)
# Adds the line of another plot, note in the first plot that the ylim is set,
# the ylim is imperative for the second line to be visible on the graph in this
# case

legend(
  'topleft',
  legend = c('CAD/USD', 'EUR/USD'),
  col = c('blue', 'red'),
  lty = c(1, 2),
  lwd = 3
)

title(main = 'Exchange Rate')

# Seasonal Decomposition

# A time series with additive trend, seasonal, and irregular components can be decomposed
# using the stl() function.

# For Additive Time Series,
# Yt=St+Tt+Et
# For Multiplicative Time Series,
# Yt=St?Tt?Et -> note this is not dissimilar if we apply a logarithmic function to the multiplicative

# Note that a series with multiplicative effects can often
# by transformedinto series with additive effects through a
# blog transformation (i.e., newts <- log(myts)).
# What is stationary time series ?
#

# Seasonal Decomposition
SeasDecom <- stl(log(excaus), s.window = "period")
plot(SeasDecom, lwd = 3)

summary(SeasDecom)
SeasDecom

# NOTE: The mean of our seasDecom is not the same at all points, so there is a
# trend (the mean is shifting over time), thus it's not stationary.
#
# Forecasting generally requires stationarity of time series The distribution
# hshould be IID The population of interest MUST be the same OR different
# population with SAME distributions

# What is stationary time series ?

# A time series is said to be stationary if it holds the follwing conditions
# true The mean value of time series is constrant over time, which implies There
# is no trend The variance does not change over time Seasonality effect is
# minimal

# This means there is no trend or seasonal pattern, which makes it look like a
# random white noise irrespective of the observed time interval

# Standard forecasting requires stationarity of data

laggedex <- lag(excaus, 1)
dif <- diff(excaus, 1) # Y(t) - Y(t-1)
plot(dif)

# Converts nonstationary data to stationary data

# First we need to check whether the process is stationary (install package
# tseries)
library(tseries)

adf.test(excaus, alternative = 'stationary')
# augmented dickey-fuller test
# Fail to reject the null (p > 0.05), so it's likely non-stationary
adf.test(diff(log(excaus)), alternative = 'stationary')
# Reject the null (p <0.05), so the data is stationary

# AR(1) model with strong serial correclation
y <- rep(NA, 5 * 12)

set.seed(pi)

y[1] <- rnorm(1)

for (j in seq(2, 5 * 12, 1)) {
  y[i] <- 1 + y[i - 1] + rnorm(1)
}

y_ts <- ts(
  y,
  start = c(2008, 1),
  end = c(2012, 12),
  frequency = 12
)
plot(y_ts)
acf(y_ts)

# ACF plots: Autocorrelation function corr(Yt, Yt-1)
acf(excaus) # high serial correlation
acf(diff(log(excaus))) # explanatory value of the first term is very small (not useful)
pacf(diff(log(excaus))) # explanatory power of each isn't too bad

fit <-
  arima(log(excaus), c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12))
# first argument is the order of the Autoregressive part c=(0,...), second is
# the difference part (p) c = (..., 1,...), last is the moving average (q)
# c(...,..,1)
pred <- predict(fit, n.ahead = 2 * 12) # two years by 12 months
ts.plot(
  excaus,
  exp(pred$pred),
  log = 'y',
  lty = c(1, 3),
  col = c('black', 'blue'),
  lwd = 3
)