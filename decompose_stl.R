# Datasets in fpp library
data(package="fpp")

plot(elecequip)

# Seasonal plots
seasonplot(elecequip, year.labels=TRUE)
monthplot(elecequip)   # see manual of monthplot() for more capabilities

# Decomposition using decompose()
fit <- decompose(elecequip)
str(fit)
plot(fit)
plot(fit$trend)

plot(elecequip)
lines(fit$trend, col="red")

# Decomposition using stl()
fit <- stl(elecequip, s.window=7)
str(fit)
plot(fit)

# More stl() capabilities
plot(stl(elecequip, s.window="periodic"))

plot(elecequip)
lines(fit$time.series[, "trend"], col="green", lwd=2)  # Galime naudoti "trend" arba 2
lines(seasadj(fit), col="red", lwd=2)
lines(fit$time.series[, "trend"] + fit$time.series[, "remainder"], col="magenta", lwd=2)

# The result of stl() can be used for forcasting
fc <- forecast(fit, h=36)
plot(fc)
str(fc)
fc$mean

# Additive and multiplicative UKgas dekomposition
plot(UKgas)
plot(decompose(UKgas))
plot(decompose(UKgas, type="multiplicative"))

# stl() does not implement multiplicative decomposition
# This should be implemented manually
plot(stl(UKgas, s.window=7))
plot(stl(log(UKgas), s.window=7))

