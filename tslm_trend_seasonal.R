library(fpp)


# Parametric trend and seasonal component estimation
plot(austourists)

fit <- tslm(austourists~trend)
summary(fit)
plot(forecast(fit, h=36))

fit <- tslm(austourists~trend+season)
summary(fit)
plot(forecast(fit, h=36))

fit <- tslm(austourists~trend+I(trend^2))
summary(fit)
plot(forecast(fit, h=36))
