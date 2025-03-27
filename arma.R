library(fpp)

##
## Example 1. If time series is stationary
##

myts <- insurance[,"Quotes"]
plot(myts)

acf(myts)
pacf(myts)

# Usefull way of ploting time series + ACF + PACF
tsdisplay(myts)

# Test if time series is white noise
Box.test(myts, type="Ljung-Box", lag=10, fitdf=0) # fitdf=p+q

# Stationarity tests (notice the contadiction in results)
adf.test(myts)    # Augmented Dickey-Fuller H0:integrated HA:stationary
kpss.test(myts)   # Kwiatkowski-Phillips-Schmidt-Shin (KPSS) H0:stationary  

# AR(1) = ARIMA(1, 0, 0)
fitarma <- arima(myts, c(1, 0, 0)) 
fitarma
summary(fitarma)
coeftest(fitarma)

# R-squared
1 - var(residuals(fitarma)) / var(myts)

# Residuals of ARMA diagnostics
tsdisplay(residuals(fitarma))
Box.test(residuals(fitarma), type="Ljung-Box", lag=10, fitdf=1) # fitdf=p+q

plot(forecast(fitarma, h=16))

# Automatic order selection
fitauto <- auto.arima(myts, trace=TRUE) 
fitauto <- auto.arima(myts, seasonal=FALSE, trace=TRUE) 
# Alternative way to remove seasonal information: myts <- ts(insurance[,"Quotes"])
fitauto
summary(fitauto)

tsdisplay(residuals(fitauto))
Box.test(residuals(fitauto), type="Ljung-Box", lag=10, fitdf=2) # fitdf=p+q

##
## Example 2. Time series has trend
## ARMA model will be used for residuals of some trend removal method
##
## Non-seasonal time series for Australia international visitors
##

? austa
plot(austa)

# Clear trend is visible
# Let's try to remove linear trend
fitlm <- tslm(austa~trend)
summary(fitlm)


# We will analyse residuals as time series (variable tr)

# Plot residuals
tr <- residuals(fitlm)
plot(tr)
abline(h=0, col="grey")

tsdisplay(tr)

# Let's use ARIMA for prediction for remainder (residuals of linear trend)
fitarma <- arima(tr, c(1, 0, 0)) 
fitarma
summary(fitarma)
coeftest(fitarma)

fitarma <- arima(tr, c(1, 0, 0), include.mean=FALSE) 
fitarma
coeftest(fitarma)

fitarma <- auto.arima(tr, trace=TRUE) 
fitarma
coeftest(fitarma)

# Residuals of ARMA diagnostics
tsdisplay(residuals(fitarma))

# R-squared
1 - var(residuals(fitarma)) / var(tr)

plot(tr, xlim=c(1980, 2025))
lines(forecast(fitarma, h=20)$mean, col="red")

# Forecast of original time series
plot(austa, xlim=c(1995, 2020), ylim=c(3, 8))
lines(forecast(fitlm, h=10)$mean, col="red")
lines(forecast(fitlm, h=10)$mean + forecast(fitarma, h=10)$mean, col="blue")

#
# Using of ARMA models for forecasting requires to make 
# time series stationary, i.e., to remove trend and seasonality.
# Workflow of forecasting:
#  * detect if trend and seasonality exist
#  * remove trend and seasonality (if it exist)
#  * use ARMA model for the remaining part of time series
#  * test if residuals of ARMA model are white noise
#  * do ARMA forecasting
#  * add ARMA forecast to the forecast of trend and seasonality (if used)
#    to obtain a forecast of original time series
#
# TASK 1: try to fit ARMA model for ts(usconsumption[, "consumption"])
#
# TASK 2: try to fit ARMA model for ts(usconsumption[, "income"])
#
# TASK 3: try to fit ARMA model for residuals of ETS model for usmelec
#
