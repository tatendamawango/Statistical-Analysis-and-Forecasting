# Load library containing required datasets and functions
library("fpp")

### Simple exponential smoothing ###

# Plot original data
? oil
oildata <- window(oil, start=1985)
plot(oildata, ylab="Oil (millions of tonnes)", xlab="Year", xlim=c(1985, 2015))

# Fit SES model and plot forecast
fit <- ses(oildata, h=5)
lines(fit$mean, col="blue", type="o")

lines(fitted(fit), col="blue", type="o")

fit

fit$model

#
# Meaning of alpha parameter
#
fit1 <- ses(oildata, alpha=0.2, initial="simple", h=5)
fit2 <- ses(oildata, alpha=0.6, initial="simple", h=5)

# Draw fitted curve
plot(oildata, ylab="Oil (millions of tonnes)", xlab="Year", xlim=c(1985, 2015))
lines(fitted(fit1), col="blue", type="o")
lines(fitted(fit2), col="red", type="o")

# Draw future forecasts
lines(fit1$mean, col="blue", type="o")
lines(fit2$mean, col="red", type="o")

legend("topleft",lty=1, col=c(1,"blue","red","green"),
  c("data", expression(alpha == 0.2), expression(alpha == 0.6)), pch=1)

### Holt's method (double exponential smoothing) ###

# Fit three models using same paramater values, but different trend types
air <- window(ausair, start=1980, end=1999)
fit1 <- holt(air, alpha=0.8, beta=0.2, h=10)
fit2 <- holt(air, alpha=0.8, beta=0.2, initial="simple", exponential=TRUE, h=10)  # simple is required for exponential model
fit3 <- holt(air, alpha=0.8, beta=0.2, damped=TRUE, h=10)

# Plot original data, smoothed data, and forecasts
plot(air, xlim=c(1985, 2009), ylim=c(10, 60), type="o", ylab="Air passengers in Australia (millions)", xlab="Year")
lines(fitted(fit1), col="red")
lines(fitted(fit2), col="blue")
lines(fitted(fit3), col="green")

lines(fit1$mean, col="red", type="o")
lines(fit2$mean, col="blue", type="o")
lines(fit3$mean, col="green", type="o")

legend("topleft", lty=1, col=c("black", "red", "blue", "green"),
     c("Data", "Linear trend", "Exponential trend", "Additive damped trend"))

lines(ausair)

# Let's estimate accuracy
accuracy(fit1)
accuracy(fit2)
accuracy(fit3)

accuracy(fit1, ausair)
accuracy(fit2, ausair)
accuracy(fit3, ausair)

# Make and plot two models for seasonal data
aust <- window(austourists, start=2005)
fit1 <- hw(aust, seasonal="additive")

# Plot the models
plot(fit1, ylab="International visitor night in Australia (millions)",
     PI=FALSE, type="o", fcol="white", xlab="Year")
lines(fitted(fit1), col="red", lty=2)
lines(fit1$mean, type="o", col="red")

# Print model fit1 summary
summary(fit1)

states <- fit1$model$states[,1:3]
colnames(states) <- c("level", "slope", "seasonal")
plot(states, xlab="Year")
fit1$model$state[,1:3]

### ETS model ###

fitets <- ets(austourists, model="ZZZ")
fitets
fitets$method

# Forecast time series
fc <- forecast(fitets)
fc
plot(fc, ylab="International visitor night in Australia (millions)")

# Plot model decomposition
plot(fitets)
