# Load package 'fpp'
library("fpp")


ausbeer
plot(ausbeer)

# Filter some time window
ausbeer[1:10]
ausbeer[1990:2004]

beer <- window(ausbeer, start=1990, end=2004.999)
beer

plot(beer)

# Test a few simple forecasting methods, print and plot results
beerfit1 <- meanf(beer, h=12)
beerfit1
plot(beerfit1)

beerfit2 <- naive(beer, h=12)
beerfit2
plot(beerfit2)

beerfit3 <- snaive(beer, h=12)
beerfit3
plot(beerfit3)

beerfit4 <- rwf(beer, drift=TRUE, h=12)
beerfit4
plot(beerfit4)

# Draw plots
plot(beerfit1, PI=FALSE, main="Forecasts for quarterly beer production")
lines(beerfit2$mean, col=2)   # You can also use col="red"
lines(beerfit3$mean, col=3)
lines(beerfit4$mean, col=6)
legend("topright", c("Mean forecast", "Naive forecast", "Seasonal naive", "RW with drift"), col=c(4, 2, 3, 6), lty=1)

# Add original data line
lines(window(ausbeer, start=2005, end=2007.999))

# Extracting forecast values only
str(beerfit1)
beerfit1$mean

# Accuracy estimation
accuracy(beerfit1)      
accuracy(beerfit1, ausbeer)
accuracy(beerfit1, window(ausbeer, start=2005)) 
# i.e. accuracy() function selects same time values for accuracy estimation

# Exercise: calculate RMSE for every forecast and select most accurate method

# RMSE calculation manually 
sqrt(mean(residuals(beerfit1)^2))

# Exercise: 
# Variable dj (of library fpp) contains values of Dow-Jones index. 
# 1) Try simple forecasting methods for this time series
# 2) Estimate quality using holdout sample and select the most precise method


#
# Time-series cross-validation using tsCV()
#

beerl <- window(ausbeer, start=1974)
plot(beerl)

e <- tsCV(beerl, meanf, h=12, initial=20)    # meanf()
fc.error.mean <- sqrt(colMeans(e^2, na.rm=TRUE))
plot(1:12, fc.error.mean, type="o", xlab="Horizon", ylab="RMSE", main="Mean forecast error")

e <- tsCV(beerl, naive, h=12, initial=20)    # naive()
fc.error.naive <- sqrt(colMeans(e^2, na.rm=TRUE))
plot(1:12, fc.error.naive, type="o", xlab="Horizon", ylab="RMSE", main="Naive forecast error")

e <- tsCV(beerl, snaive, h=12, initial=20)    # snaive()
fc.error.snaive <- sqrt(colMeans(e^2, na.rm=TRUE))
plot(1:12, fc.error.snaive, type="o", xlab="Horizon", ylab="RMSE", main="Seasonal naive forecast error")

plot(forecast(stl(beer2, s.window=7), h=12))


my.stlf <- function(data, h) {
  decomp <- stl(data, s.window=7)
  return( forecast(decomp, h=h) )
}

e <- tsCV(beerl, my.stlf, h=12, initial=20)    # forecast(stl())
fc.error.mystlf <- sqrt(colMeans(e^2, na.rm=TRUE))
plot(1:12, fc.error.mystlf, type="o", xlab="Horizon", ylab="RMSE", main="My stl() decomposition forecast error")
