#
# EXAMPLE. Regression model for time series containing structural change
#

plot(ausbeer)
fit2 <- tslm(ausbeer~trend+season)
summary(fit2)

# Plot forecast based on tslm()
fc2 <- forecast(fit2, h=20)
plot(fc2)

# Global linear trend model visualisation
plot(ausbeer[1:211], type="l")
abline(fit2)  # abline() is not able to produce a line on original time series plot because of x-axis limits

#
# Simple change if we can cut old date out of a sample
#
ausbeertail <- window(ausbeer, start=1980)
fittail <- tslm(ausbeertail~trend+season)
summary(fittail)
fctail <- forecast(fittail, h=20)
plot(fctail)


#
# Alternative model. Using dummy variables for structural change
#

# Define dummy variable on structural change point
dummy <- rep(1, 211)
dummy[1:70] <- 0
dummy

# Define dummy variable multiplied by t
dummytrend <- dummy * 1:211
dummytrend

fit3 <- tslm(ausbeer~trend+season+dummy+dummytrend)
summary(fit3)

# Trendline visualisation
plot(ausbeer[1:211], type="l")

# Linear trend line before structural change point
abline(a=fit3$coefficients[1], b=fit3$coefficients[2], col="red")
abline(a=fit3$coefficients["(Intercept)"], b=fit3$coefficients["trend"], col="red")

# Linear trend after structural change point
abline(a=fit3$coefficients["(Intercept)"] + fit3$coefficients["dummy"], 
       b=fit3$coefficients["trend"] + fit3$coefficients["dummytrend"], col="blue")

# Let's do forecasting
futuredummy <- data.frame(dummy=rep(1, 30), dummytrend=212:241)
fcdummy <- forecast(fit3, newdata=futuredummy, h=30)
plot(fcdummy)

