library(fpp)


plot(insurance, main="Insurance advertising and quotations", xlab="Year")

#
# EXAMPLE. Trend and seasonal component modelling using linear regression
#
fitcomp <- tslm(insurance[,1]~trend+season)
summary(fitcomp)


#
# EXAMPLE. Exogenous variable modelling using linear regression
#
fitadv <- tslm(insurance[,1]~insurance[,2])
summary(fitadv)

acf(residuals(fitadv))


#
# EXAMPLE. Trend, seasonality, and exogenous variable model
#
fitmix <- tslm(insurance[,1]~trend+season+insurance[,2])
summary(fitmix)

acf(residuals(fitmix))

# Forecasting
fccomp <- forecast(fitcomp, h=36)
plot(fccomp)

fcmix <- forecast(fitmix, h=36)  # ERROR! Why?
